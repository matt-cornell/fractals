use super::*;
use serde::{Deserialize, Serialize};
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll, Waker};

pub struct EditorState {
    file_res: Result<bool, String>,
    edit_res: Result<(), String>,
    edit_buf: String,
    save_config: Option<Pin<Box<dyn Future<Output = Option<rfd::FileHandle>>>>>,
    load_config: Option<Pin<Box<dyn Future<Output = Option<rfd::FileHandle>>>>>,
}
impl Default for EditorState {
    fn default() -> Self {
        Self {
            file_res: Ok(false),
            edit_res: Ok(()),
            edit_buf: String::new(),
            save_config: None,
            load_config: None,
        }
    }
}

pub fn show(app: &mut App, ui: &mut egui::Ui) {
    if app.z.changed() || app.c.changed() || app.p.changed() {
        let res = serde_json::to_string_pretty(&AppState {
            exponent: app.common.exponent,
            depth: app.common.depth,
            renorm: app.common.renorm,
            z: app.z.param_str.clone(),
            c: app.c.param_str.clone(),
            p: app.p.param_str.clone(),
            palette: Some(if let Some(lower) = &app.common.lower {
                PaletteSerde::Split {
                    upper: PaletteData {
                        exponential: app.common.upper.exponential,
                        stops: app.common.upper.stops.clone(),
                    },
                    lower: PaletteData {
                        exponential: lower.exponential,
                        stops: lower.stops.clone(),
                    },
                }
            } else {
                PaletteSerde::Single {
                    palette: PaletteData {
                        exponential: app.common.upper.exponential,
                        stops: app.common.upper.stops.clone(),
                    },
                }
            }),
        });
        if let Ok(new) = res {
            app.edit.edit_buf = new;
        }
    }
    ui.horizontal(|ui| {
        if ui.button("Save").clicked() {
            app.edit.save_config = Some(Box::pin(
                rfd::AsyncFileDialog::new()
                    .add_filter("JSON", &["json"])
                    .save_file(),
            ));
        }
        if ui.button("Load").clicked() {
            app.edit.load_config = Some(Box::pin(
                rfd::AsyncFileDialog::new()
                    .add_filter("JSON", &["json"])
                    .pick_file(),
            ));
        }
    });
    let force_changed;
    match app.edit.file_res {
        Ok(ref mut ch) => force_changed = std::mem::take(ch),
        Err(ref err) => {
            force_changed = false;
            let mut clicked = false;
            add_error(err, ui, Some(&mut || clicked = true));
            if clicked {
                app.edit.file_res = Ok(false);
            }
        }
    }
    if let Err(err) = &app.edit.edit_res {
        let visuals = &ui.style().visuals;
        ui.label(
            egui::RichText::new(format!("Invalid data: {err}"))
                .color(visuals.error_fg_color)
                .background_color(visuals.extreme_bg_color),
        );
    }
    if scrollable_text(&mut app.edit.edit_buf, ui).changed() || force_changed {
        match serde_json::from_str::<AppState>(&app.edit.edit_buf) {
            Ok(state) => {
                app.edit.edit_res = Ok(());
                app.common.exponent = state.exponent;
                if app.common.exponent % 1.0 != 0.0 {
                    app.integer_exp = false;
                }
                app.common.depth = state.depth;
                app.common.renorm = state.renorm;
                app.z.param_str = state.z;
                app.c.param_str = state.c;
                app.p.param_str = state.p;
                app.z.parse_str();
                app.c.parse_str();
                app.p.parse_str();
                app.z.mark_changed();
                app.c.mark_changed();
                app.p.mark_changed();
                let sort = state.palette.is_some();
                if let Some(p) = state.palette {
                    match p {
                        PaletteSerde::Single { palette } => {
                            app.common.upper.exponential = palette.exponential;
                            app.common.upper.edit = palette.stops;
                            app.common.lower = None;
                        }
                        PaletteSerde::Split { upper, lower } => {
                            app.common.upper.exponential = upper.exponential;
                            app.common.upper.edit = upper.stops;
                            let lower_palette = app.common.lower.get_or_insert(Palette {
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
                app.common.regenerate(sort);
            }
            Err(err) => app.edit.edit_res = Err(err.to_string()),
        }
    }
    if let Some(fut) = &mut app.edit.save_config {
        if let Poll::Ready(handle) = fut.as_mut().poll(&mut Context::from_waker(Waker::noop())) {
            app.edit.save_config = None;
            if let Some(handle) = handle {
                if let Err(err) = std::fs::write(handle.path(), &app.edit.edit_buf) {
                    app.edit.file_res = Err(format!("Failed to save config: {err}"))
                } else {
                    app.edit.file_res = Ok(false);
                }
            }
        } else {
            ui.ctx().request_repaint_after(Duration::from_millis(100));
        }
    }
    if let Some(fut) = &mut app.edit.load_config {
        if let Poll::Ready(handle) = fut.as_mut().poll(&mut Context::from_waker(Waker::noop())) {
            app.edit.load_config = None;
            if let Some(handle) = handle {
                match std::fs::read_to_string(handle.path()) {
                    Ok(buf) => {
                        app.edit.edit_buf = buf;
                        app.edit.file_res = Ok(true);
                    }
                    Err(err) => app.edit.file_res = Err(format!("Failed to load config: {err}")),
                }
            }
        } else {
            ui.ctx().request_repaint_after(Duration::from_millis(100));
        }
    }
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
