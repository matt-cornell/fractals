#![allow(clippy::type_complexity, clippy::ptr_arg)]

use eframe::egui;
use egui::Color32;
use num_complex::{Complex64, ComplexFloat};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::fmt::Write;
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll, Wake, Waker};
use std::thread::Thread;
use std::time::Duration;

mod math;

macro_rules! load_text {
    ($path:literal, $style:expr) => {{
        static LOCK: std::sync::LazyLock<eframe::egui::text::LayoutJob> =
            std::sync::LazyLock::new(|| $crate::math::parse_text(include_str!($path)));
        $crate::math::adapt_parsed(&*LOCK, $style)
    }};
}

fn fractal_depth(
    o: f64,
    mut z: Complex64,
    c: Complex64,
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
    pub fn bw(depth: usize) -> Self {
        let mut this = Self {
            edit: vec![(Color32::BLACK, 0.0), (Color32::WHITE, 1.0)],
            stops: vec![(Color32::BLACK, 0.0), (Color32::WHITE, 1.0)],
            exponential: false,
            palette: Vec::new(),
        };
        if depth > 0 {
            this.regenerate(depth, false);
        }
        this
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
    #[serde(default)]
    renorm: bool,
    z: String,
    c: String,
    #[serde(rename = "P")]
    p: String,
    #[serde(flatten, with = "serde_palette")]
    palette: Option<PaletteSerde>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FractalPlane {
    Z,
    C,
    P,
}

struct ThreadWaker {
    thread: Thread,
    notified: AtomicBool,
}
impl Wake for ThreadWaker {
    fn wake(self: Arc<Self>) {
        self.notified.store(true, Ordering::Release);
        self.thread.unpark();
    }
    fn wake_by_ref(self: &Arc<Self>) {
        self.notified.store(true, Ordering::Release);
        self.thread.unpark();
    }
}

const SPECIAL_BASE: usize = usize::MAX >> 2;
const READY: usize = SPECIAL_BASE;
const WAITING: usize = SPECIAL_BASE + 1;
const CANCELLING: usize = SPECIAL_BASE >> 1;

struct ImageData {
    resolution: usize,
    buffer: Vec<Color32>,
    handle: Option<egui::TextureHandle>,
    changed: bool,
    param: Complex64,
    param_str: String,
    invalid_param: bool,
    save_fut: Option<(Pin<Box<dyn Future<Output = Option<rfd::FileHandle>>>>, bool)>,
    save_res: Result<(), String>,
    marker: bool,
}
impl ImageData {
    fn parse_str(&mut self) {
        if let Ok(p) = self.param_str.parse() {
            self.param = p;
            self.invalid_param = false;
            if self.marker {
                self.mark_changed();
            }
        } else {
            self.invalid_param = true;
        }
    }
    #[inline(always)]
    fn mark_changed(&mut self) {
        self.changed = true;
    }
    fn update(
        &mut self,
        plane: FractalPlane,
        common: &CommonData,
        zcp: [Complex64; 3],
        ctx: &egui::Context,
    ) {
        if !self.changed {
            return;
        }
        self.changed = false;
        render_fractal(
            zcp,
            plane,
            self.marker.then_some(self.param),
            self.resolution,
            common,
            &mut self.buffer,
            None,
        );
        let img = egui::ColorImage {
            size: [self.resolution; 2],
            source_size: egui::Vec2::splat(self.resolution as f32),
            pixels: self.buffer.clone(),
        };
        if let Some(handle) = &mut self.handle {
            handle.set(img, egui::TextureOptions::NEAREST);
        } else {
            self.handle = Some(ctx.load_texture(
                format!("{plane:?}-plane"),
                img,
                egui::TextureOptions::NEAREST,
            ));
        }
    }
    fn show(
        &mut self,
        label: &str,
        plane: FractalPlane,
        common: &CommonData,
        zcp: [Complex64; 3],
        ui: &mut egui::Ui,
    ) -> bool {
        let mut updated = false;
        ui.horizontal(|ui| {
            ui.label(format!("{label}: "));
            ui.vertical(|ui| {
                if ui
                    .add(egui::TextEdit::singleline(&mut self.param_str).desired_width(50.0))
                    .changed()
                {
                    self.parse_str();
                    if !self.invalid_param {
                        updated = true;
                    }
                }
                if self.invalid_param {
                    add_error(&"Invalid parameter", ui, None);
                }
            });
            if ui.checkbox(&mut self.marker, "Show marker").changed() {
                self.mark_changed();
            }
        });
        self.changed |= ui
            .add(
                egui::Slider::new(&mut self.resolution, 0..=4096)
                    .clamping(egui::SliderClamping::Never)
                    .logarithmic(true)
                    .text("Resolution"),
            )
            .changed();
        if let Err(err) = &self.save_res {
            let mut clicked = false;
            add_error(err, ui, Some(&mut || clicked = true));
            if clicked {
                self.save_res = Ok(());
            }
        }
        let mut img = ui
            .image(self.handle.as_ref().unwrap())
            .interact(egui::Sense::drag());
        if self.marker {
            img = img.interact(egui::Sense::click_and_drag());
            if img.clicked_by(egui::PointerButton::Primary)
                || img.dragged_by(egui::PointerButton::Primary)
            {
                let scale = 4.0 / (self.resolution as f64);
                if let Some(pos) = img.interact_pointer_pos() {
                    let p = pos - img.rect.min;
                    let x = p.x as f64 * scale - 2.0;
                    let y = p.y as f64 * scale - 2.0;
                    let res = self.resolution as f64;
                    self.param = Complex64::new((x * res).round() / res, (-y * res).round() / res);
                    self.param_str.clear();
                    let _ = write!(self.param_str, "{}", self.param);
                    self.mark_changed();
                    updated = true;
                }
            }
        }
        img.context_menu(|ui| {
            if ui.button("Copy").clicked() {
                let mut buffer = self.buffer.clone();
                if self.marker {
                    remove_marker(zcp, plane, self.param, self.resolution, common, &mut buffer);
                }
                ui.ctx().copy_image(egui::ColorImage {
                    size: [self.resolution; 2],
                    source_size: egui::Vec2::splat(self.resolution as f32),
                    pixels: buffer,
                });
            }
            if ui.button("Save").clicked() {
                self.save_fut = Some((
                    Box::pin(
                        rfd::AsyncFileDialog::new()
                            .add_filter("Images", &["png"])
                            .set_file_name("fractal.png")
                            .save_file(),
                    ),
                    false,
                ));
            }
            if ui.button("Copy with marker").clicked() {
                let mut buffer = self.buffer.clone();
                if !self.marker {
                    add_marker(self.param, self.resolution, &mut buffer);
                }
                ui.ctx().copy_image(egui::ColorImage {
                    size: [self.resolution; 2],
                    source_size: egui::Vec2::splat(self.resolution as f32),
                    pixels: buffer,
                });
            }
            if ui.button("Save with marker").clicked() {
                self.save_fut = Some((
                    Box::pin(
                        rfd::AsyncFileDialog::new()
                            .add_filter("Images", &["png"])
                            .set_file_name("fractal.png")
                            .save_file(),
                    ),
                    true,
                ));
            }
        });
        updated
    }
    fn poll_fut(
        &mut self,
        plane: FractalPlane,
        common: &CommonData,
        zcp: [Complex64; 3],
        ctx: &egui::Context,
    ) {
        if let Some((fut, marker)) = &mut self.save_fut {
            let marker = *marker;
            if let Poll::Ready(handle) = fut.as_mut().poll(&mut Context::from_waker(Waker::noop()))
            {
                self.save_fut = None;
                'save: {
                    let Some(handle) = handle else {
                        break 'save;
                    };
                    let Ok(file) = std::fs::File::create(handle.path()).inspect_err(|err| {
                        self.save_res = Err(format!("Failed to open file: {err}"))
                    }) else {
                        break 'save;
                    };
                    let mut encoder =
                        png::Encoder::new(file, self.resolution as _, self.resolution as _);
                    encoder.set_color(png::ColorType::Rgba);
                    let Ok(mut writer) = encoder.write_header().inspect_err(|err| {
                        self.save_res = Err(format!("Failed to save image: {err}"))
                    }) else {
                        break 'save;
                    };
                    let mut buffer = Vec::new();
                    let pixels = if self.marker != marker {
                        buffer.clone_from(&self.buffer);
                        if self.marker {
                            remove_marker(
                                zcp,
                                plane,
                                self.param,
                                self.resolution,
                                common,
                                &mut buffer,
                            );
                        } else {
                            add_marker(self.param, self.resolution, &mut buffer);
                        }
                        &buffer
                    } else {
                        &self.buffer
                    };
                    if let Err(err) = writer.write_image_data(bytemuck::cast_slice(pixels)) {
                        self.save_res = Err(format!("Failed to save image: {err}"));
                        break 'save;
                    }
                    if let Err(err) = writer.finish() {
                        self.save_res = Err(format!("Failed to save image: {err}"));
                        break 'save;
                    }
                }
            } else {
                ctx.request_repaint_after(Duration::from_millis(100));
            }
        }
    }
}
impl Default for ImageData {
    fn default() -> Self {
        Self {
            resolution: 256,
            buffer: Vec::new(),
            handle: None,
            changed: true,
            param: Complex64::ZERO,
            param_str: Complex64::ZERO.to_string(),
            invalid_param: false,
            save_fut: None,
            save_res: Ok(()),
            marker: true,
        }
    }
}

#[derive(Clone)]
struct CommonData {
    exponent: f64,
    depth: usize,
    renorm: bool,
    upper: Palette,
    lower: Option<Palette>,
}
impl CommonData {
    fn regenerate(&mut self, sort: bool) {
        self.upper.regenerate(self.depth, sort);
        if let Some(lower) = &mut self.lower {
            lower.regenerate(self.depth, sort);
        }
    }
}
impl Default for CommonData {
    fn default() -> Self {
        Self {
            exponent: 2.0,
            depth: 100,
            renorm: false,
            upper: Palette::bw(100),
            lower: None,
        }
    }
}

fn add_error(msg: &dyn ToString, ui: &mut egui::Ui, clickable: Option<&mut dyn FnMut()>) {
    let visuals = &ui.style().visuals;
    let base = egui::Label::new(
        egui::RichText::new(msg.to_string())
            .color(visuals.error_fg_color)
            .background_color(visuals.extreme_bg_color),
    );
    if let Some(cb) = clickable {
        if ui.add(base.sense(egui::Sense::click())).clicked() {
            cb();
        }
    } else {
        ui.add(base);
    }
}

fn show_param(ui: &mut egui::Ui, name: &str, plane: &mut ImageData, others: [&mut ImageData; 2]) {
    ui.horizontal(|ui| {
        ui.label(format!("{name}: "));
        ui.vertical(|ui| {
            if ui
                .add(egui::TextEdit::singleline(&mut plane.param_str).desired_width(50.0))
                .changed()
            {
                plane.parse_str();
                if !plane.invalid_param {
                    for p in others {
                        p.mark_changed();
                    }
                }
            }
            if plane.invalid_param {
                add_error(&"Invalid parameter", ui, None);
            }
        });
    });
}

fn render_fractal(
    [z, c, p]: [Complex64; 3],
    plane: FractalPlane,
    target: Option<Complex64>,
    resolution: usize,
    common: &CommonData,
    buffer: &mut Vec<Color32>,
    term: Option<&(dyn Fn() -> bool + Send + Sync)>,
) -> bool {
    buffer.resize(resolution * resolution, Color32::PLACEHOLDER);
    let scale = 4.0 / (resolution as f64);
    let target = target.map(|t| {
        (
            ((t.re + 2.0) / scale) as usize,
            ((-t.im + 2.0) / scale) as usize,
        )
    });
    let work = |(n, px): (usize, &mut Color32)| {
        let (y, x) = num_integer::div_rem(n, resolution);
        if target.is_some_and(|(tx, ty)| {
            (x == tx && y.abs_diff(ty) < 5) || (y == ty && x.abs_diff(tx) < 5)
        }) {
            *px = Color32::RED;
        } else {
            let x = x as f64 * scale - 2.0;
            let y = y as f64 * scale - 2.0;
            let v = Complex64::new(x, -y);
            let (mut z, mut c, mut p) = (z, c, p);
            match plane {
                FractalPlane::Z => z = v,
                FractalPlane::C => c = v,
                FractalPlane::P => p = v,
            }
            let (d, z) = fractal_depth(common.exponent, z, c, p, common.depth);
            let mut d = if common.renorm {
                (((d + 1) as f64 - z.abs().max(1.0).ln().max(1.0).ln()) / std::f64::consts::LN_2)
                    as usize
            } else {
                d
            };
            if d >= common.upper.palette.len() {
                d = common.upper.palette.len() - 1;
            }
            let upper = common.upper.palette[d];
            let color = if let Some(lower) = &common.lower {
                upper.lerp_to_gamma(lower.palette[d], y.mul_add(0.25, 0.5) as _)
            } else {
                upper
            };

            *px = color;
        }
    };
    if let Some(t) = term {
        buffer.par_iter_mut().enumerate().any(|v| {
            work(v);
            t()
        })
    } else {
        buffer.par_iter_mut().enumerate().for_each(work);
        false
    }
}

fn remove_marker(
    [z, c, p]: [Complex64; 3],
    plane: FractalPlane,
    target: Complex64,
    resolution: usize,
    common: &CommonData,
    buffer: &mut Vec<Color32>,
) {
    let scale = 4.0 / (resolution as f64);
    let (tx, ty) = (
        ((target.re + 2.0) / scale) as usize,
        ((-target.im + 2.0) / scale) as usize,
    );
    let mut fill_px = |x: usize, y: usize| {
        let idx = y * resolution + x;
        let x = x as f64 * scale - 2.0;
        let y = y as f64 * scale - 2.0;
        let v = Complex64::new(x, -y);
        let (mut z, mut c, mut p) = (z, c, p);
        match plane {
            FractalPlane::Z => z = v,
            FractalPlane::C => c = v,
            FractalPlane::P => p = v,
        }
        let (d, z) = fractal_depth(common.exponent, z, c, p, common.depth);
        let mut d = if common.renorm {
            (((d + 1) as f64 - z.abs().max(1.0).ln().max(1.0).ln()) / std::f64::consts::LN_2)
                as usize
        } else {
            d
        };
        if d >= common.upper.palette.len() {
            d = common.upper.palette.len() - 1;
        }
        let upper = common.upper.palette[d];
        let color = if let Some(lower) = &common.lower {
            upper.lerp_to_gamma(lower.palette[d], y.mul_add(0.25, 0.5) as _)
        } else {
            upper
        };

        buffer[idx] = color;
    };
    let xmin = tx.saturating_sub(5);
    let xmax = tx.saturating_add(5).min(resolution - 1);
    let ymin = ty.saturating_sub(5);
    let ymax = ty.saturating_add(5).min(resolution - 1);
    for x in xmin..=xmax {
        fill_px(x, ty);
    }
    for y in ymin..=ymax {
        if y != ty {
            fill_px(tx, y);
        }
    }
}

fn add_marker(target: Complex64, resolution: usize, buffer: &mut Vec<Color32>) {
    let scale = 4.0 / (resolution as f64);
    let (tx, ty) = (
        ((target.re + 2.0) / scale) as usize,
        ((-target.im + 2.0) / scale) as usize,
    );
    let xmin = tx.saturating_sub(5);
    let xmax = tx.saturating_add(5).min(resolution - 1);
    let ymin = ty.saturating_sub(5);
    let ymax = ty.saturating_add(5).min(resolution - 1);
    buffer[(ty * resolution + xmin)..=(ty * resolution + xmax)].fill(Color32::RED);
    for y in ymin..=ymax {
        buffer[y * resolution + tx] = Color32::RED;
    }
}

fn scrollable_text(buf: &mut String, ui: &mut egui::Ui) -> egui::Response {
    let available = ui.available_rect_before_wrap();
    let where_to_put_background = ui.painter().add(egui::Shape::Noop);
    let sao = egui::ScrollArea::both().show(ui, |ui| {
        ui.set_min_size(available.size());
        ui.set_clip_rect(available);
        ui.add_sized(
            available.size(),
            egui::TextEdit::multiline(buf).code_editor().frame(false),
        )
    });
    let visuals = ui.visuals();
    let widget = ui.style().interact(&sao.inner);
    let background = visuals.text_edit_bg_color();
    let stroke = if sao.inner.has_focus() {
        visuals.selection.stroke
    } else {
        widget.bg_stroke
    };
    let shape = egui::epaint::RectShape::new(
        available,
        widget.corner_radius,
        background,
        stroke,
        egui::StrokeKind::Inside,
    );
    ui.painter().set(where_to_put_background, shape);
    sao.inner
}

fn main() {
    let mut z = ImageData::default();
    let mut c = ImageData::default();
    let mut p = ImageData::default();
    let mut common = CommonData::default();
    let mut integer_exp = true;
    let mut file_res = Ok::<_, String>(false);
    let mut edit_res = Ok::<_, String>(());
    let mut edit_buf = String::new();
    let mut export_sel = FractalPlane::Z;
    let mut export_size = 8192;
    let export_state = Arc::new(AtomicUsize::new(READY));
    let export_res = Arc::new(Mutex::new(Ok(())));
    let mut save_config = None;
    let mut load_config = None;
    eframe::run_simple_native(
        "Fractal Explorer",
        eframe::NativeOptions {
            centered: true,
            viewport: egui::ViewportBuilder::default().with_inner_size(egui::vec2(1280.0, 900.0)),
            ..Default::default()
        },
        move |ctx, _| {
            egui::Window::new("Editor").default_size(egui::vec2(250.0, 250.0)).show(ctx, |ui| {
                if z.changed || c.changed || p.changed {
                    let res = serde_json::to_string_pretty(&AppState {
                        exponent: common.exponent,
                        depth: common.depth,
                        renorm: common.renorm,
                        z: z.param_str.clone(),
                        c: c.param_str.clone(),
                        p: p.param_str.clone(),
                        palette: Some(if let Some(lower) = &common.lower {
                            PaletteSerde::Split {
                                upper: PaletteData {
                                    exponential: common.upper.exponential,
                                    stops: common.upper.stops.clone(),
                                },
                                lower: PaletteData {
                                    exponential: lower.exponential,
                                    stops: lower.stops.clone(),
                                },
                            }
                        } else {
                            PaletteSerde::Single {
                                palette: PaletteData {
                                    exponential: common.upper.exponential,
                                    stops: common.upper.stops.clone(),
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
                        let mut clicked = false;
                        add_error(err, ui, Some(&mut || clicked = true));
                        if clicked {
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
                if scrollable_text(&mut edit_buf, ui).changed() || force_changed {
                    match serde_json::from_str::<AppState>(&edit_buf) {
                        Ok(state) => {
                            edit_res = Ok(());
                            common.exponent = state.exponent;
                            if common.exponent % 1.0 != 0.0 {
                                integer_exp = false;
                            }
                            common.depth = state.depth;
                            common.renorm = state.renorm;
                            z.param_str = state.z;
                            c.param_str = state.c;
                            p.param_str = state.p;
                            z.parse_str();
                            c.parse_str();
                            p.parse_str();
                            z.mark_changed();
                            c.mark_changed();
                            p.mark_changed();
                            let sort = state.palette.is_some();
                            if let Some(p) = state.palette {
                                match p {
                                    PaletteSerde::Single { palette } => {
                                        common.upper.exponential = palette.exponential;
                                        common.upper.edit = palette.stops;
                                        common.lower = None;
                                    }
                                    PaletteSerde::Split { upper, lower } => {
                                        common.upper.exponential = upper.exponential;
                                        common.upper.edit = upper.stops;
                                        let lower_palette = common.lower.get_or_insert(Palette {
                                            edit: Vec::new(),
                                            stops: Vec::new(),
                                            exponential: false,
                                            palette: Vec::new(),
                                        });
                                        lower_palette.exponential = lower.exponential;
                                        lower_palette.edit = lower.stops;
                                    }
                                }
                            }
                            common.regenerate(sort);
                        }
                        Err(err) => edit_res = Err(err.to_string()),
                    }
                }
            });
            z.update(FractalPlane::Z, &common, [z.param, c.param, p.param], ctx);
            c.update(FractalPlane::C, &common, [z.param, c.param, p.param], ctx);
            p.update(FractalPlane::P, &common, [z.param, c.param, p.param], ctx);
            egui::Window::new("Params").show(ctx, |ui| {
                ui.horizontal(|ui| {
                    if ui
                        .add(
                            egui::Slider::new(&mut common.exponent, -5.0..=5.0)
                                .max_decimals(if integer_exp { 0 } else { 2 })
                                .text("Exponent"),
                        )
                        .changed()
                    {
                        z.mark_changed();
                        c.mark_changed();
                        p.mark_changed();
                    }
                    ui.checkbox(&mut integer_exp, "Integer");
                });
                if ui
                    .add(
                        egui::Slider::new(&mut common.depth, 1..=1000)
                            .logarithmic(true)
                            .text("Depth"),
                    )
                    .changed()
                {
                    common.upper.regenerate(common.depth, false);
                    if let Some(lower) = &mut common.lower {
                        lower.regenerate(common.depth, false);
                    }
                    z.mark_changed();
                    c.mark_changed();
                    p.mark_changed();
                }
                show_param(ui, "z", &mut z, [&mut c, &mut p]);
                show_param(ui, "c", &mut c, [&mut p, &mut z]);
                show_param(ui, "P", &mut p, [&mut z, &mut c]);
            });
            egui::Window::new("Colors").show(ctx, |ui| {
                let renorm_box = ui.checkbox(&mut common.renorm, "Renormalize");
                if renorm_box.hovered() {
                    renorm_box.show_tooltip_text("Renormalization can help reduce banding in the images, and the results look slightly more vibrant. Try it to see the difference!");
                }
                let mut changed = renorm_box.changed();
                let mut show_lower = common.lower.is_some();
                let split_grad_box = ui.checkbox(&mut show_lower, "Split Gradient");
                if split_grad_box.hovered() {
                    split_grad_box.show_tooltip_text("Use a linear blend of upper and lower gradients rather than just one");
                }
                changed |= split_grad_box.changed();
                changed |= egui::CollapsingHeader::new(if common.lower.is_some() {
                    "Upper"
                } else {
                    "Gradient"
                })
                .id_salt("Upper")
                .default_open(true)
                .show(ui, |ui| show_picker(ui, &mut common.upper))
                .body_returned
                .unwrap_or(false);
                if show_lower {
                    let lower = common.lower.get_or_insert_with(|| common.upper.clone());
                    changed |= egui::CollapsingHeader::new("Lower")
                        .default_open(true)
                        .show(ui, |ui| show_picker(ui, lower))
                        .body_returned
                        .unwrap_or(false);
                } else {
                    common.lower = None;
                }
                if changed {
                    common.regenerate(true);
                    z.mark_changed();
                    c.mark_changed();
                    p.mark_changed();
                }
            });
            egui::Window::new("Export").show(ctx, |ui| {
                let state = export_state.load(Ordering::Acquire);
                egui::ComboBox::new("Fractal Plane", "Fractal Plane")
                    .selected_text(match export_sel {
                        FractalPlane::Z => "z",
                        FractalPlane::C => "c",
                        FractalPlane::P => "P",
                    })
                    .show_ui(ui, |ui| {
                        ui.selectable_value(&mut export_sel, FractalPlane::Z, "z");
                        ui.selectable_value(&mut export_sel, FractalPlane::C, "c");
                        ui.selectable_value(&mut export_sel, FractalPlane::P, "P");
                    });
                ui.add_enabled(
                    state == READY,
                    egui::Slider::new(&mut export_size, 0..=10000)
                        .clamping(egui::SliderClamping::Never)
                        .logarithmic(true),
                );

                match state {
                    READY => {
                        if ui.button("Export").clicked() {
                            *export_res.lock().unwrap() = Ok(());
                            let state = Arc::clone(&export_state);
                            let common = common.clone();
                            let res = Arc::clone(&export_res);
                            let zcp = [z.param, c.param, p.param];
                            std::thread::spawn(move || {
                                state.store(0, Ordering::Release);
                                let mut buffer =
                                    vec![Color32::PLACEHOLDER; export_size * export_size];
                                let broke = render_fractal(
                                    zcp,
                                    export_sel,
                                    None,
                                    export_size,
                                    &common,
                                    &mut buffer,
                                    Some(&|| state.fetch_add(1, Ordering::AcqRel) >= CANCELLING),
                                );
                                if broke {
                                    state.store(READY, Ordering::Release);
                                    return;
                                }
                                state.store(WAITING, Ordering::Release);
                                let fut = rfd::AsyncFileDialog::new()
                                    .add_filter("Images", &["png"])
                                    .set_file_name("fractal.png")
                                    .save_file();
                                let mut fut = std::pin::pin!(fut);
                                let waker_handle = Arc::new(ThreadWaker {
                                    thread: std::thread::current(),
                                    notified: AtomicBool::new(true),
                                });
                                let waker = waker_handle.clone().into();
                                let mut cx = Context::from_waker(&waker);
                                let handle;
                                loop {
                                    if let Poll::Ready(h) = fut.as_mut().poll(&mut cx) {
                                        handle = h;
                                        break;
                                    }
                                    if state
                                        .compare_exchange(
                                            CANCELLING,
                                            READY,
                                            Ordering::AcqRel,
                                            Ordering::Acquire,
                                        )
                                        .is_ok()
                                    {
                                        return;
                                    }
                                    let n = waker_handle.notified.swap(false, Ordering::AcqRel);
                                    if !n {
                                        std::thread::park_timeout(Duration::from_millis(100));
                                    }
                                }
                                let Some(handle) = handle else {
                                    state.store(READY, Ordering::Release);
                                    return;
                                };

                                'save: {
                                    let Ok(file) = std::fs::File::create(handle.path())
                                        .inspect_err(|err| {
                                            *res.lock().unwrap() =
                                                Err(format!("Failed to open file: {err}"))
                                        })
                                    else {
                                        break 'save;
                                    };
                                    let mut encoder =
                                        png::Encoder::new(file, export_size as _, export_size as _);
                                    encoder.set_color(png::ColorType::Rgba);
                                    let Ok(mut writer) =
                                        encoder.write_header().inspect_err(|err| {
                                            *res.lock().unwrap() =
                                                Err(format!("Failed to save image: {err}"))
                                        })
                                    else {
                                        break 'save;
                                    };
                                    if let Err(err) =
                                        writer.write_image_data(bytemuck::cast_slice(&buffer))
                                    {
                                        *res.lock().unwrap() =
                                            Err(format!("Failed to save image: {err}"));
                                        break 'save;
                                    }
                                    if let Err(err) = writer.finish() {
                                        *res.lock().unwrap() =
                                            Err(format!("Failed to save image: {err}"));
                                        break 'save;
                                    }
                                }

                                state.store(READY, Ordering::Release);
                            });
                        }
                    }
                    WAITING => {
                        ui.horizontal(|ui| {
                            ui.label("Ready to save");
                            if ui.button("Cancel").clicked() {
                                export_state.store(CANCELLING, Ordering::Release);
                            }
                        });
                    }
                    CANCELLING..SPECIAL_BASE => {
                        ui.horizontal(|ui| {
                            ui.spinner();
                            ui.label("Cancelling...");
                        });
                    }
                    step => {
                        let progress = step as f32 / (export_size * export_size) as f32;
                        ui.horizontal(|ui| {
                            ui.spinner();
                            ui.label(format!(
                                "Processing {}/{} ({:.1}%)",
                                step,
                                export_size * export_size,
                                progress * 100.0
                            ));
                            if ui.button("Cancel").clicked() {
                                export_state.store(CANCELLING, Ordering::Release);
                            }
                        });
                        ui.add(egui::ProgressBar::new(progress));
                    }
                }
                if let Err(err) = &*export_res.lock().unwrap() {
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
                        file_res = Ok(false);
                    }
                }
            });
            egui::Window::new("Z-Plane").show(ctx, |ui| {
                if z.show("z", FractalPlane::Z, &common, [z.param, c.param, p.param], ui) {
                    c.mark_changed();
                    p.mark_changed();
                }
            });
            egui::Window::new("C-Plane").show(ctx, |ui| {
                if c.show("c", FractalPlane::C, &common, [z.param, c.param, p.param], ui) {
                    p.mark_changed();
                    z.mark_changed();
                }
            });
            egui::Window::new("P-Plane").show(ctx, |ui| {
                if p.show("P", FractalPlane::P, &common, [z.param, c.param, p.param], ui) {
                    z.mark_changed();
                    c.mark_changed();
                }
            });
            egui::Window::new("Recommended Reading").default_size(egui::vec2(300.0, 400.0)).default_open(false).default_pos(egui::pos2(1000.0, 800.0)).show(ctx, |ui| {
                egui::ScrollArea::vertical().show(ui, |ui| {
                    ui.heading("Iterative Fractals");
                    ui.label(load_text!("info/iterative-fractal.txt", &ui.style()));
                    ui.heading("The Mandelbrot Set");
                    ui.label(load_text!("info/mandelbrot-set.txt", &ui.style()));
                    ui.heading("Julia Sets");
                    ui.label(load_text!("info/julia-sets.txt", &ui.style()));
                    ui.heading("Higher Exponents");
                    ui.label(load_text!("info/higher-exponents.txt", &ui.style()));
                    ui.heading("Phoenix Fractals");
                    ui.label(load_text!("info/phoenix-fractals.txt", &ui.style()));
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
                } else {
                    ctx.request_repaint_after(Duration::from_millis(100));
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
                } else {
                    ctx.request_repaint_after(Duration::from_millis(100));
                }
            }
            z.poll_fut(FractalPlane::Z, &common, [z.param, c.param, p.param], ctx);
            c.poll_fut(FractalPlane::C, &common, [z.param, c.param, p.param], ctx);
            p.poll_fut(FractalPlane::P, &common, [z.param, c.param, p.param], ctx);
        },
    )
    .unwrap();
}
