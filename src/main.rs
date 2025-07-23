use eframe::egui;
use egui::Color32;
use num_complex::{Complex64, ComplexFloat};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::future::Future;
use std::task::{Context, Poll, Waker};

fn fractal_depth(
    mut z: Complex64,
    c: Complex64,
    o: f64,
    p: Complex64,
    depth: usize,
) -> (usize, Complex64) {
    let mut last = Complex64::ZERO;
    let bound = if o < 0.0 {
        2.0 * std::f64::consts::SQRT_2
    } else {
        2.0
    };
    for i in 0..depth {
        if z.abs() > bound {
            return (i, z);
        }
        let old_z = z;
        z = z.powf(o) + p * last + c;
        last = old_z;
    }
    (depth, z)
}

#[derive(Clone)]
struct Palette {
    edit: Vec<(Color32, f32)>,
    stops: Vec<(Color32, f32)>,
    exponential: bool,
    palette: Vec<Color32>,
}
impl Palette {
    pub fn bw() -> Self {
        Self {
            edit: vec![(Color32::BLACK, 0.0), (Color32::WHITE, 1.0)],
            stops: vec![(Color32::BLACK, 0.0), (Color32::WHITE, 1.0)],
            exponential: false,
            palette: Vec::new(),
        }
    }
    pub fn regenerate(&mut self, depth: usize, sort: bool) {
        self.stops.clone_from(&self.edit);
        self.palette.clear();
        self.palette.reserve(depth + 1);
        if sort {
            self.stops.sort_by(|a, b| a.1.total_cmp(&b.1));
        }
        for i in 0..=depth {
            let d = if self.exponential {
                1.0 - 0.5f32.powf(i as f32 / depth as f32)
            } else {
                i as f32 / depth as f32
            };
            let idx = self.stops.binary_search_by(|(_, k)| k.total_cmp(&d));
            match idx {
                Ok(idx) => self.palette.push(self.stops[idx].0),
                Err(idx) => {
                    if idx == 0 {
                        self.palette.push(self.stops[0].0);
                    } else if idx == self.stops.len() {
                        self.palette.push(self.stops[self.stops.len() - 1].0);
                    } else {
                        let (a, ka) = self.stops[idx - 1];
                        let (b, kb) = self.stops[idx];
                        self.palette.push(a.lerp_to_gamma(b, (d - ka) / (kb - ka)));
                    }
                }
            }
        }
    }
    pub fn respace(&mut self) {
        self.edit.sort_by(|a, b| a.1.total_cmp(&b.1));
        let step = ((self.edit.len() - 1) as f32).recip();
        let mut val = 0.0;
        for (_color, k) in &mut self.edit {
            *k = val;
            val += step;
        }
    }
}

fn show_picker(ui: &mut egui::Ui, palette: &mut Palette) -> bool {
    let mut changed = ui
        .checkbox(&mut palette.exponential, "Exponential")
        .changed();
    if ui.button("Respace").clicked() {
        palette.respace();
        changed = true;
    }
    let mut to_remove = Vec::new();
    let can_remove = palette.edit.len() > 1;
    for (i, (color, k)) in palette.edit.iter_mut().enumerate() {
        ui.horizontal(|ui| {
            changed |= ui.color_edit_button_srgba(color).changed();
            if ui
                .add(egui::Slider::new(k, 0.0..=1.0).text("Stop"))
                .changed()
            {
                changed = true;
            }
            if ui.add_enabled(can_remove, egui::Button::new("X")).clicked() {
                to_remove.push(i);
                changed = true;
            }
        });
    }
    let mut i = 0;
    palette.edit.retain(|_| {
        if to_remove.contains(&i) {
            i += 1;
            false
        } else {
            i += 1;
            true
        }
    });
    if ui.button("Add Stop").clicked() {
        palette.edit.push((Color32::WHITE, 1.0));
        changed = true;
    }
    changed
}

mod serde_stops {
    use eframe::egui::ecolor::ParseHexColorError;
    use eframe::egui::Color32;
    use serde::de::{Error, Unexpected};
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use std::fmt::Write;
    struct ColorShim(Color32, f32);
    impl<'de> Deserialize<'de> for ColorShim {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            const EXPECTED: &str = "A hex-formatted color, '@', and a position";
            let val: &str = Deserialize::deserialize(deserializer)?;
            let idx = val
                .find('@')
                .ok_or_else(|| Error::invalid_value(Unexpected::Str(val), &EXPECTED))?;
            let color = Color32::from_hex(&val[..idx]).map_err(|err| match err {
                ParseHexColorError::MissingHash => {
                    Error::invalid_value(Unexpected::Str(val), &EXPECTED)
                }
                ParseHexColorError::InvalidLength => {
                    Error::invalid_value(Unexpected::Str(val), &EXPECTED)
                }
                ParseHexColorError::InvalidInt(_) => {
                    Error::invalid_value(Unexpected::Str(val), &EXPECTED)
                }
            })?;
            let pos = val[(idx + 1)..]
                .parse()
                .map_err(|_| Error::invalid_value(Unexpected::Str(val), &EXPECTED))?;
            Ok(Self(color, pos))
        }
    }
    #[allow(clippy::ptr_arg)]
    pub fn serialize<S: Serializer>(
        vec: &Vec<(Color32, f32)>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        vec.iter()
            .map(|(c, p)| {
                let mut out = c.to_hex();
                let _ = write!(out, "@{p:.4}");
                while out.ends_with('0') {
                    out.pop();
                }
                if out.ends_with('.') {
                    out.push('0');
                }
                out
            })
            .collect::<Vec<_>>()
            .serialize(serializer)
    }
    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Vec<(Color32, f32)>, D::Error> {
        let raw = Vec::<ColorShim>::deserialize(deserializer)?;
        Ok(raw.into_iter().map(|c| (c.0, c.1)).collect())
    }
}

mod serde_palette {
    use super::PaletteSerde;
    use serde::de::{Error, MapAccess, Visitor};
    use serde::ser::SerializeStruct;
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S: Serializer>(
        value: &Option<PaletteSerde>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        match value {
            None => serializer.serialize_none(),
            Some(PaletteSerde::Single { palette }) => {
                let mut s = serializer.serialize_struct("Palette", 1)?;
                s.serialize_field("palette", palette)?;
                s.end()
            }
            Some(PaletteSerde::Split { upper, lower }) => {
                let mut s = serializer.serialize_struct("Palette", 2)?;
                s.serialize_field("upper", upper)?;
                s.serialize_field("lower", lower)?;
                s.end()
            }
        }
    }
    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Option<PaletteSerde>, D::Error> {
        #[derive(Deserialize)]
        #[serde(rename_all = "snake_case", field_identifier)]
        enum Field {
            Palette,
            Upper,
            Lower,
            Unknown(serde::de::IgnoredAny),
        }

        struct PaletteVisitor;
        impl<'de> Visitor<'de> for PaletteVisitor {
            type Value = Option<PaletteSerde>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("palette data")
            }
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut palette = None;
                let mut upper = None;
                let mut lower = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Palette => {
                            if palette.is_some() {
                                return Err(Error::duplicate_field("palette"));
                            }
                            palette = Some(map.next_value()?)
                        }
                        Field::Upper => {
                            if upper.is_some() {
                                return Err(Error::duplicate_field("upper"));
                            }
                            upper = Some(map.next_value()?);
                        }
                        Field::Lower => {
                            if lower.is_some() {
                                return Err(Error::duplicate_field("lower"));
                            }
                            lower = Some(map.next_value()?);
                        }
                        Field::Unknown(_) => {}
                    }
                }
                match (palette, upper, lower) {
                    (_, Some(upper), Some(lower)) => Ok(Some(PaletteSerde::Split { upper, lower })),
                    (Some(palette), _, _) => Ok(Some(PaletteSerde::Single { palette })),
                    (None, None, None) => Ok(None),
                    (None, None, Some(_)) => Err(Error::missing_field("upper")),
                    (None, Some(_), None) => Err(Error::missing_field("lower")),
                }
            }
        }

        deserializer.deserialize_map(PaletteVisitor)
    }
}

#[derive(Serialize, Deserialize)]
struct PaletteData {
    #[serde(default)]
    exponential: bool,
    #[serde(with = "serde_stops")]
    stops: Vec<(Color32, f32)>,
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum PaletteSerde {
    Single {
        palette: PaletteData,
    },
    Split {
        upper: PaletteData,
        lower: PaletteData,
    },
}

#[derive(Serialize, Deserialize)]
struct AppState {
    exponent: f64,
    depth: usize,
    c: String,
    #[serde(rename = "P")]
    p: String,
    #[serde(flatten, with = "serde_palette")]
    palette: Option<PaletteSerde>,
}

fn main() {
    let mut multibrot_resolution = 256;
    let mut phoenix_resolution = 256;
    let mut hyperjulia_resolution = 512;
    let mut c = Complex64::ZERO;
    let mut exponent = 2.0;
    let mut p = Complex64::ZERO;
    let mut depth = 100usize;
    let mut integer_exp = true;
    let mut show_marker = true;
    let mut renorm = false;
    let mut multibrot_buffer = Vec::new();
    let mut hyperjulia_buffer = Vec::new();
    let mut phoenix_buffer = Vec::new();
    let mut multibrot_handle = None;
    let mut hyperjulia_handle = None;
    let mut phoenix_handle = None;
    let mut update_multibrot = true;
    let mut update_phoenix = true;
    let mut update_hyperjulia = true;
    let mut c_str = c.to_string();
    let mut p_str = p.to_string();
    let mut invalid_c = false;
    let mut invalid_p = false;
    let mut upper_palette = Palette::bw();
    let mut lower_palette = None::<Palette>;
    let mut file_res = Ok::<_, String>(false);
    let mut edit_res = Ok::<_, String>(());
    let mut edit_buf = String::new();
    upper_palette.regenerate(depth, false);
    let mut save_config = None;
    let mut load_config = None;
    let mut save_brot = None;
    let mut save_phoenix = None;
    let mut save_julia = None;
    let mut save_brot_res: Result<(), String> = Ok(());
    let mut save_phoenix_res: Result<(), String> = Ok(());
    let mut save_julia_res: Result<(), String> = Ok(());
    eframe::run_simple_native(
        "Hyperjulia",
        eframe::NativeOptions {
            centered: true,
            viewport: egui::ViewportBuilder::default().with_inner_size(egui::vec2(1280.0, 900.0)),
            ..Default::default()
        },
        move |ctx, _| {
            egui::Window::new("Editor").show(ctx, |ui| {
                if update_multibrot || update_hyperjulia {
                    let res = serde_json::to_string_pretty(&AppState {
                        exponent,
                        depth,
                        c: c_str.clone(),
                        p: p_str.clone(),
                        palette: Some(if let Some(lower) = &lower_palette {
                            PaletteSerde::Split {
                                upper: PaletteData {
                                    exponential: upper_palette.exponential,
                                    stops: upper_palette.stops.clone(),
                                },
                                lower: PaletteData {
                                    exponential: lower.exponential,
                                    stops: lower.stops.clone(),
                                },
                            }
                        } else {
                            PaletteSerde::Single {
                                palette: PaletteData {
                                    exponential: upper_palette.exponential,
                                    stops: upper_palette.stops.clone(),
                                },
                            }
                        }),
                    });
                    if let Ok(new) = res {
                        edit_buf = new;
                    }
                }
                ui.horizontal(|ui| {
                    if ui.button("Save").clicked() {
                        save_config = Some(Box::pin(
                            rfd::AsyncFileDialog::new()
                                .add_filter("JSON", &["json"])
                                .save_file(),
                        ));
                    }
                    if ui.button("Load").clicked() {
                        load_config = Some(Box::pin(
                            rfd::AsyncFileDialog::new()
                                .add_filter("JSON", &["json"])
                                .pick_file(),
                        ));
                    }
                });
                let force_changed;
                match file_res {
                    Ok(ref mut ch) => force_changed = std::mem::take(ch),
                    Err(ref err) => {
                        force_changed = false;
                        let visuals = &ui.style().visuals;
                        if ui
                            .add(
                                egui::Label::new(
                                    egui::RichText::new(format!("Invalid data: {err}"))
                                        .color(visuals.error_fg_color)
                                        .background_color(visuals.extreme_bg_color),
                                )
                                .sense(egui::Sense::click()),
                            )
                            .clicked()
                        {
                            file_res = Ok(false);
                        }
                    }
                }
                if let Err(err) = &edit_res {
                    let visuals = &ui.style().visuals;
                    ui.label(
                        egui::RichText::new(format!("Invalid data: {err}"))
                            .color(visuals.error_fg_color)
                            .background_color(visuals.extreme_bg_color),
                    );
                }
                if ui.code_editor(&mut edit_buf).changed() || force_changed {
                    match serde_json::from_str::<AppState>(&edit_buf) {
                        Ok(state) => {
                            edit_res = Ok(());
                            exponent = state.exponent;
                            if exponent % 1.0 != 0.0 {
                                integer_exp = false;
                            }
                            depth = state.depth;
                            c_str = state.c;
                            p_str = state.p;
                            update_multibrot = true;
                            update_hyperjulia = true;
                            update_phoenix = true;
                            if let Ok(new_c) = c_str.parse() {
                                c = new_c;
                                invalid_c = false;
                            } else {
                                invalid_c = true;
                            }
                            if let Ok(new_p) = p_str.parse() {
                                p = new_p;
                                invalid_p = false;
                            } else {
                                invalid_p = true;
                            }
                            if let Some(p) = state.palette {
                                match p {
                                    PaletteSerde::Single { palette } => {
                                        upper_palette.exponential = palette.exponential;
                                        upper_palette.edit = palette.stops;
                                        upper_palette.regenerate(depth, true);
                                        lower_palette = None;
                                    }
                                    PaletteSerde::Split { upper, lower } => {
                                        upper_palette.exponential = upper.exponential;
                                        upper_palette.edit = upper.stops;
                                        upper_palette.regenerate(depth, true);
                                        let lower_palette = lower_palette.get_or_insert(Palette {
                                            edit: Vec::new(),
                                            stops: Vec::new(),
                                            exponential: false,
                                            palette: Vec::new(),
                                        });
                                        lower_palette.exponential = lower.exponential;
                                        lower_palette.edit = lower.stops;
                                        lower_palette.regenerate(depth, true);
                                    }
                                }
                            }
                        }
                        Err(err) => edit_res = Err(err.to_string()),
                    }
                }
            });
            egui::Window::new("Display").show(ctx, |ui| {
                if ui
                    .add(
                        egui::Slider::new(&mut multibrot_resolution, 10..=10000)
                            .clamping(egui::SliderClamping::Never)
                            .logarithmic(true)
                            .text("Multibrot Resolution"),
                    )
                    .changed()
                {
                    update_multibrot = true;
                }
                if ui
                    .add(
                        egui::Slider::new(&mut phoenix_resolution, 10..=10000)
                            .clamping(egui::SliderClamping::Never)
                            .logarithmic(true)
                            .text("Phoenix Resolution"),
                    )
                    .changed()
                {
                    update_phoenix = true;
                }
                if ui
                    .add(
                        egui::Slider::new(&mut hyperjulia_resolution, 10..=10000)
                            .clamping(egui::SliderClamping::Never)
                            .logarithmic(true)
                            .text("Hyperjulia Resolution"),
                    )
                    .changed()
                {
                    update_hyperjulia = true;
                }
                if ui.checkbox(&mut show_marker, "Show Markers").changed() {
                    update_multibrot = true;
                    update_phoenix = true;
                }
                if ui.checkbox(&mut renorm, "Renorm Hyperjulia").changed() {
                    update_hyperjulia = true;
                }
            });
            egui::Window::new("Params").show(ctx, |ui| {
                ui.horizontal(|ui| {
                    if ui
                        .add(
                            egui::Slider::new(&mut exponent, -5.0..=5.0)
                                .max_decimals(if integer_exp { 0 } else { 2 })
                                .text("Exponent"),
                        )
                        .changed()
                    {
                        update_multibrot = true;
                        update_hyperjulia = true;
                        update_phoenix = true;
                    }
                    ui.checkbox(&mut integer_exp, "Integer");
                });
                ui.horizontal(|ui| {
                    ui.label("c: ");
                    ui.vertical(|ui| {
                        if ui
                            .add(egui::TextEdit::singleline(&mut c_str).desired_width(50.0))
                            .changed()
                        {
                            if let Ok(new_c) = c_str.parse() {
                                c = new_c;
                                update_hyperjulia = true;
                                if show_marker {
                                    update_multibrot = true;
                                }
                                update_phoenix = true;
                                invalid_c = false;
                            } else {
                                invalid_c = true;
                            }
                        }
                        if invalid_c {
                            let visuals = &ui.style().visuals;
                            ui.label(
                                egui::RichText::new("Invalid parameter")
                                    .color(visuals.error_fg_color)
                                    .background_color(visuals.extreme_bg_color),
                            );
                        }
                    });
                });
                ui.horizontal(|ui| {
                    ui.label("P: ");
                    ui.vertical(|ui| {
                        if ui
                            .add(egui::TextEdit::singleline(&mut p_str).desired_width(50.0))
                            .changed()
                        {
                            if let Ok(new_p) = p_str.parse() {
                                p = new_p;
                                update_multibrot = true;
                                update_hyperjulia = true;
                                if show_marker {
                                    update_phoenix = true;
                                }
                                invalid_p = false;
                            } else {
                                invalid_p = true;
                            }
                        }
                        if invalid_p {
                            let visuals = &ui.style().visuals;
                            ui.label(
                                egui::RichText::new("Invalid parameter")
                                    .color(visuals.error_fg_color)
                                    .background_color(visuals.extreme_bg_color),
                            );
                        }
                    });
                });
                if ui
                    .add(
                        egui::Slider::new(&mut depth, 1..=1000)
                            .logarithmic(true)
                            .text("Depth"),
                    )
                    .changed()
                {
                    upper_palette.regenerate(depth, false);
                    if let Some(lower) = &mut lower_palette {
                        lower.regenerate(depth, false);
                    }
                    update_multibrot = true;
                    update_hyperjulia = true;
                    update_phoenix = true;
                }
            });
            egui::Window::new("Colors").show(ctx, |ui| {
                let mut changed = egui::CollapsingHeader::new(if lower_palette.is_some() {
                    "Upper"
                } else {
                    "Gradient"
                })
                .id_salt("Upper")
                .show(ui, |ui| show_picker(ui, &mut upper_palette))
                .body_returned
                .unwrap_or(false);
                let mut show_lower = lower_palette.is_some();
                changed |= ui.checkbox(&mut show_lower, "Lower").changed();
                if show_lower {
                    let lower = lower_palette.get_or_insert_with(|| upper_palette.clone());
                    changed |= ui
                        .collapsing("Lower", |ui| show_picker(ui, lower))
                        .body_returned
                        .unwrap_or(false);
                } else {
                    lower_palette = None;
                }
                if changed {
                    upper_palette.regenerate(depth, true);
                    if let Some(lower) = &mut lower_palette {
                        lower.regenerate(depth, true);
                    }
                    update_multibrot = true;
                    update_hyperjulia = true;
                }
            });
            let multibrot = multibrot_handle.get_or_insert_with(|| {
                ctx.load_texture(
                    "multibrot",
                    egui::ColorImage::example(),
                    egui::TextureOptions::NEAREST,
                )
            });
            let phoenix = phoenix_handle.get_or_insert_with(|| {
                ctx.load_texture(
                    "phoenix",
                    egui::ColorImage::example(),
                    egui::TextureOptions::NEAREST,
                )
            });
            let hyperjulia = hyperjulia_handle.get_or_insert_with(|| {
                ctx.load_texture(
                    "hyperjulia",
                    egui::ColorImage::example(),
                    egui::TextureOptions::NEAREST,
                )
            });
            if update_multibrot {
                update_multibrot = false;
                multibrot_buffer.resize(
                    multibrot_resolution * multibrot_resolution,
                    Color32::PLACEHOLDER,
                );
                let scale = 4.0 / (multibrot_resolution as f64);
                let target = show_marker.then(|| {
                    (
                        ((c.re + 2.0) / scale) as usize,
                        ((-c.im + 2.0) / scale) as usize,
                    )
                });
                multibrot_buffer
                    .par_iter_mut()
                    .enumerate()
                    .for_each(|(n, px)| {
                        let (y, x) = num_integer::div_rem(n, multibrot_resolution);
                        if target.is_some_and(|(tx, ty)| {
                            (x == tx && y.abs_diff(ty) < 5) || (y == ty && x.abs_diff(tx) < 5)
                        }) {
                            *px = Color32::RED;
                        } else {
                            let x = x as f64 * scale - 2.0;
                            let y = y as f64 * scale - 2.0;
                            let (d, _) = fractal_depth(
                                Complex64::ZERO,
                                Complex64::new(x, -y),
                                exponent,
                                p,
                                depth,
                            );
                            let upper = upper_palette.palette[d];
                            let color = if let Some(lower) = &lower_palette {
                                upper.lerp_to_gamma(lower.palette[d], y.mul_add(0.25, 0.5) as _)
                            } else {
                                upper
                            };

                            *px = color;
                        }
                    });
                multibrot.set(
                    egui::ColorImage {
                        size: [multibrot_resolution; 2],
                        source_size: egui::Vec2::splat(multibrot_resolution as f32),
                        pixels: multibrot_buffer.clone(),
                    },
                    egui::TextureOptions::NEAREST,
                );
            }
            if update_phoenix {
                update_phoenix = false;
                phoenix_buffer.resize(
                    multibrot_resolution * multibrot_resolution,
                    Color32::PLACEHOLDER,
                );
                let scale = 4.0 / (phoenix_resolution as f64);
                let target = show_marker.then(|| {
                    (
                        ((p.re + 2.0) / scale) as usize,
                        ((-p.im + 2.0) / scale) as usize,
                    )
                });
                phoenix_buffer
                    .par_iter_mut()
                    .enumerate()
                    .for_each(|(n, px)| {
                        let (y, x) = num_integer::div_rem(n, phoenix_resolution);
                        if target.is_some_and(|(tx, ty)| {
                            (x == tx && y.abs_diff(ty) < 5) || (y == ty && x.abs_diff(tx) < 5)
                        }) {
                            *px = Color32::RED;
                        } else {
                            let x = x as f64 * scale - 2.0;
                            let y = y as f64 * scale - 2.0;
                            let (d, _) = fractal_depth(
                                Complex64::ZERO,
                                c,
                                exponent,
                                Complex64::new(x, -y),
                                depth,
                            );
                            let upper = upper_palette.palette[d];
                            let color = if let Some(lower) = &lower_palette {
                                upper.lerp_to_gamma(lower.palette[d], y.mul_add(0.25, 0.5) as _)
                            } else {
                                upper
                            };

                            *px = color;
                        }
                    });
                phoenix.set(
                    egui::ColorImage {
                        size: [phoenix_resolution; 2],
                        source_size: egui::Vec2::splat(phoenix_resolution as f32),
                        pixels: phoenix_buffer.clone(),
                    },
                    egui::TextureOptions::NEAREST,
                );
            }
            if update_hyperjulia {
                update_hyperjulia = false;
                hyperjulia_buffer.resize(
                    hyperjulia_resolution * hyperjulia_resolution,
                    Color32::PLACEHOLDER,
                );
                let scale = 4.0 / (hyperjulia_resolution as f64);
                hyperjulia_buffer
                    .par_iter_mut()
                    .enumerate()
                    .for_each(|(n, px)| {
                        let (y, x) = num_integer::div_rem(n, hyperjulia_resolution);
                        let x = x as f64 * scale - 2.0;
                        let y = y as f64 * scale - 2.0;
                        let z = Complex64::new(x, -y);
                        let (d, z) = fractal_depth(z, c, exponent, p, depth);
                        let mut d = if renorm {
                            (((d + 1) as f64 - z.abs().max(1.0).ln().max(1.0).ln())
                                / std::f64::consts::LN_2) as usize
                        } else {
                            d
                        };
                        if d >= upper_palette.palette.len() {
                            d = upper_palette.palette.len();
                        }
                        let upper = upper_palette.palette[d];
                        let color = if let Some(lower) = &lower_palette {
                            upper.lerp_to_gamma(lower.palette[d], y.mul_add(0.25, 0.5) as _)
                        } else {
                            upper
                        };
                        *px = color;
                    });
                hyperjulia.set(
                    egui::ColorImage {
                        size: [hyperjulia_resolution; 2],
                        source_size: egui::Vec2::splat(hyperjulia_resolution as f32),
                        pixels: hyperjulia_buffer.clone(),
                    },
                    egui::TextureOptions::NEAREST,
                )
            }
            egui::Window::new("Multibrot").show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label("c: ");
                    ui.vertical(|ui| {
                        if ui
                            .add(egui::TextEdit::singleline(&mut c_str).desired_width(50.0))
                            .changed()
                        {
                            if let Ok(new_c) = c_str.parse() {
                                c = new_c;
                                update_hyperjulia = true;
                                if show_marker {
                                    update_multibrot = true;
                                }
                                update_phoenix = true;
                                invalid_c = false;
                            } else {
                                invalid_c = true;
                            }
                        }
                        if invalid_c {
                            let visuals = &ui.style().visuals;
                            ui.label(
                                egui::RichText::new("Invalid parameter")
                                    .color(visuals.error_fg_color)
                                    .background_color(visuals.extreme_bg_color),
                            );
                        }
                    });
                });
                if let Err(err) = &save_brot_res {
                    let visuals = &ui.style().visuals;
                    if ui
                        .add(
                            egui::Label::new(
                                egui::RichText::new(err)
                                    .color(visuals.error_fg_color)
                                    .background_color(visuals.extreme_bg_color),
                            )
                            .sense(egui::Sense::click()),
                        )
                        .clicked()
                    {
                        save_brot_res = Ok(());
                    }
                }
                let mut img = ui.image(&*multibrot);
                if show_marker {
                    img = img.interact(egui::Sense::click_and_drag());
                    let scale = 4.0 / (multibrot_resolution as f64);
                    if let Some(pos) = img.interact_pointer_pos() {
                        let p = pos - img.rect.min;
                        let x = p.x as f64 * scale - 2.0;
                        let y = p.y as f64 * scale - 2.0;
                        c = Complex64::new((x * 100.0).round() * 0.01, (y * 100.0).round() * -0.01);
                        update_hyperjulia = true;
                        update_multibrot = true;
                        update_phoenix = true;
                        c_str = c.to_string();
                    }
                } else {
                    img.interact(egui::Sense::click()).context_menu(|ui| {
                        if ui.button("Copy").clicked() {
                            ui.ctx().copy_image(egui::ColorImage {
                                size: [multibrot_resolution; 2],
                                source_size: egui::Vec2::splat(multibrot_resolution as f32),
                                pixels: multibrot_buffer.clone(),
                            });
                        }
                        if ui.button("Save").clicked() {
                            save_brot = Some(Box::pin(
                                rfd::AsyncFileDialog::new()
                                    .add_filter("Images", &["png"])
                                    .set_file_name("multibrot.png")
                                    .save_file(),
                            ));
                        }
                    });
                }
            });
            egui::Window::new("Phoenix").show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label("P: ");
                    ui.vertical(|ui| {
                        if ui
                            .add(egui::TextEdit::singleline(&mut p_str).desired_width(50.0))
                            .changed()
                        {
                            if let Ok(new_p) = p_str.parse() {
                                p = new_p;
                                update_hyperjulia = true;
                                update_multibrot = true;
                                if show_marker {
                                    update_phoenix = true;
                                }
                                invalid_p = false;
                            } else {
                                invalid_p = true;
                            }
                        }
                        if invalid_p {
                            let visuals = &ui.style().visuals;
                            ui.label(
                                egui::RichText::new("Invalid parameter")
                                    .color(visuals.error_fg_color)
                                    .background_color(visuals.extreme_bg_color),
                            );
                        }
                    });
                });
                if let Err(err) = &save_phoenix_res {
                    let visuals = &ui.style().visuals;
                    if ui
                        .add(
                            egui::Label::new(
                                egui::RichText::new(err)
                                    .color(visuals.error_fg_color)
                                    .background_color(visuals.extreme_bg_color),
                            )
                            .sense(egui::Sense::click()),
                        )
                        .clicked()
                    {
                        save_phoenix_res = Ok(());
                    }
                }
                let mut img = ui.image(&*phoenix);
                if show_marker {
                    img = img.interact(egui::Sense::click_and_drag());
                    let scale = 4.0 / (phoenix_resolution as f64);
                    if let Some(pos) = img.interact_pointer_pos() {
                        let pos = pos - img.rect.min;
                        let x = pos.x as f64 * scale - 2.0;
                        let y = pos.y as f64 * scale - 2.0;
                        p = Complex64::new((x * 100.0).round() * 0.01, (y * 100.0).round() * -0.01);
                        update_hyperjulia = true;
                        update_multibrot = true;
                        update_phoenix = true;
                        p_str = p.to_string();
                    }
                } else {
                    img.interact(egui::Sense::click()).context_menu(|ui| {
                        if ui.button("Copy").clicked() {
                            ui.ctx().copy_image(egui::ColorImage {
                                size: [phoenix_resolution; 2],
                                source_size: egui::Vec2::splat(phoenix_resolution as f32),
                                pixels: phoenix_buffer.clone(),
                            });
                        }
                        if ui.button("Save").clicked() {
                            save_phoenix = Some(Box::pin(
                                rfd::AsyncFileDialog::new()
                                    .add_filter("Images", &["png"])
                                    .set_file_name("phoenix.png")
                                    .save_file(),
                            ));
                        }
                    });
                }
            });
            egui::Window::new("Hyperjulia").show(ctx, |ui| {
                if let Err(err) = &save_julia_res {
                    let visuals = &ui.style().visuals;
                    if ui
                        .add(
                            egui::Label::new(
                                egui::RichText::new(err)
                                    .color(visuals.error_fg_color)
                                    .background_color(visuals.extreme_bg_color),
                            )
                            .sense(egui::Sense::click()),
                        )
                        .clicked()
                    {
                        save_julia_res = Ok(());
                    }
                }
                let img = ui.image(&*hyperjulia);
                img.interact(egui::Sense::click()).context_menu(|ui| {
                    if ui.button("Copy").clicked() {
                        ui.ctx().copy_image(egui::ColorImage {
                            size: [hyperjulia_resolution; 2],
                            source_size: egui::Vec2::splat(hyperjulia_resolution as f32),
                            pixels: hyperjulia_buffer.clone(),
                        });
                    }
                    if ui.button("Save").clicked() {
                        save_julia = Some(Box::pin(
                            rfd::AsyncFileDialog::new()
                                .add_filter("Images", &["png"])
                                .set_file_name("hyperjulia.png")
                                .save_file(),
                        ));
                    }
                });
            });
            if let Some(fut) = &mut save_config {
                if let Poll::Ready(handle) =
                    fut.as_mut().poll(&mut Context::from_waker(Waker::noop()))
                {
                    save_config = None;
                    if let Some(handle) = handle {
                        if let Err(err) = std::fs::write(handle.path(), &edit_buf) {
                            file_res = Err(format!("Failed to save config: {err}"))
                        } else {
                            file_res = Ok(false);
                        }
                    }
                }
            }
            if let Some(fut) = &mut load_config {
                if let Poll::Ready(handle) =
                    fut.as_mut().poll(&mut Context::from_waker(Waker::noop()))
                {
                    load_config = None;
                    if let Some(handle) = handle {
                        match std::fs::read_to_string(handle.path()) {
                            Ok(buf) => {
                                edit_buf = buf;
                                file_res = Ok(true);
                            }
                            Err(err) => file_res = Err(format!("Failed to load config: {err}")),
                        }
                    }
                }
            }
            if let Some(fut) = &mut save_brot {
                if let Poll::Ready(handle) =
                    fut.as_mut().poll(&mut Context::from_waker(Waker::noop()))
                {
                    save_brot = None;
                    'save: {
                        let Some(handle) = handle else {
                            break 'save;
                        };
                        let Ok(file) = std::fs::File::create(handle.path()).inspect_err(|err| {
                            save_brot_res = Err(format!("Failed to open file: {err}"))
                        }) else {
                            break 'save;
                        };
                        let mut encoder = png::Encoder::new(
                            file,
                            multibrot_resolution as _,
                            multibrot_resolution as _,
                        );
                        encoder.set_color(png::ColorType::Rgba);
                        let Ok(mut writer) = encoder.write_header().inspect_err(|err| {
                            save_brot_res = Err(format!("Failed to save image: {err}"))
                        }) else {
                            break 'save;
                        };
                        if let Err(err) =
                            writer.write_image_data(bytemuck::cast_slice(&multibrot_buffer))
                        {
                            save_brot_res = Err(format!("Failed to save image: {err}"));
                            break 'save;
                        }
                        if let Err(err) = writer.finish() {
                            save_brot_res = Err(format!("Failed to save image: {err}"));
                            break 'save;
                        }
                    }
                }
            }
            if let Some(fut) = &mut save_phoenix {
                if let Poll::Ready(handle) =
                    fut.as_mut().poll(&mut Context::from_waker(Waker::noop()))
                {
                    save_brot = None;
                    'save: {
                        let Some(handle) = handle else {
                            break 'save;
                        };
                        let Ok(file) = std::fs::File::create(handle.path()).inspect_err(|err| {
                            save_phoenix_res = Err(format!("Failed to open file: {err}"))
                        }) else {
                            break 'save;
                        };
                        let mut encoder = png::Encoder::new(
                            file,
                            phoenix_resolution as _,
                            phoenix_resolution as _,
                        );
                        encoder.set_color(png::ColorType::Rgba);
                        let Ok(mut writer) = encoder.write_header().inspect_err(|err| {
                            save_phoenix_res = Err(format!("Failed to save image: {err}"))
                        }) else {
                            break 'save;
                        };
                        if let Err(err) =
                            writer.write_image_data(bytemuck::cast_slice(&multibrot_buffer))
                        {
                            save_phoenix_res = Err(format!("Failed to save image: {err}"));
                            break 'save;
                        }
                        if let Err(err) = writer.finish() {
                            save_phoenix_res = Err(format!("Failed to save image: {err}"));
                            break 'save;
                        }
                    }
                }
            }
            if let Some(fut) = &mut save_julia {
                if let Poll::Ready(handle) =
                    fut.as_mut().poll(&mut Context::from_waker(Waker::noop()))
                {
                    save_julia = None;
                    'save: {
                        let Some(handle) = handle else {
                            break 'save;
                        };
                        let Ok(file) = std::fs::File::create(handle.path()).inspect_err(|err| {
                            save_julia_res = Err(format!("Failed to open file: {err}"))
                        }) else {
                            break 'save;
                        };
                        let mut encoder = png::Encoder::new(
                            file,
                            hyperjulia_resolution as _,
                            hyperjulia_resolution as _,
                        );
                        encoder.set_color(png::ColorType::Rgba);
                        let Ok(mut writer) = encoder.write_header().inspect_err(|err| {
                            save_julia_res = Err(format!("Failed to save image: {err}"))
                        }) else {
                            break 'save;
                        };
                        if let Err(err) =
                            writer.write_image_data(bytemuck::cast_slice(&hyperjulia_buffer))
                        {
                            save_julia_res = Err(format!("Failed to save image: {err}"));
                            break 'save;
                        }
                        if let Err(err) = writer.finish() {
                            save_julia_res = Err(format!("Failed to save image: {err}"));
                            break 'save;
                        }
                    }
                }
            }
        },
    )
    .unwrap();
}
