#![allow(clippy::type_complexity, clippy::ptr_arg)]

use eframe::egui;
use egui::Color32;
use num_complex::{Complex64, ComplexFloat};
use rayon::prelude::*;
use std::time::Duration;

mod editor;
mod export;
mod math;
mod plane;

macro_rules! load_text {
    ($path:literal, $style:expr) => {{
        static LOCK: std::sync::LazyLock<eframe::egui::text::LayoutJob> =
            std::sync::LazyLock::new(|| $crate::math::parse_text(include_str!($path)));
        $crate::math::adapt_parsed(&*LOCK, $style)
    }};
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

#[derive(Debug, Clone, Copy, PartialEq)]
enum FractalPlane {
    Z,
    C,
    P,
}
impl FractalPlane {
    const fn name(self) -> &'static str {
        match self {
            Self::Z => "z",
            Self::C => "c",
            Self::P => "P",
        }
    }
    const fn plane(self) -> &'static str {
        match self {
            Self::Z => "Z-Plane",
            Self::C => "C-Plane",
            Self::P => "P-Plane",
        }
    }
    fn select<T>(self, zcp: [T; 3]) -> (T, [T; 2]) {
        let [z, c, p] = zcp;
        match self {
            Self::Z => (z, [c, p]),
            Self::C => (c, [p, z]),
            Self::P => (p, [z, c]),
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

struct App {
    common: CommonData,
    z: plane::ImageData,
    c: plane::ImageData,
    p: plane::ImageData,
    edit: editor::EditorState,
    export: export::ExportState,
    integer_exp: bool,
}
impl App {
    pub fn new(cc: &eframe::CreationContext) -> Self {
        Self {
            common: CommonData::default(),
            z: plane::ImageData::new(cc.egui_ctx.load_texture(
                "Z-Plane",
                egui::ColorImage::example(),
                egui::TextureOptions::NEAREST,
            )),
            c: plane::ImageData::new(cc.egui_ctx.load_texture(
                "C-Plane",
                egui::ColorImage::example(),
                egui::TextureOptions::NEAREST,
            )),
            p: plane::ImageData::new(cc.egui_ctx.load_texture(
                "P-Plane",
                egui::ColorImage::example(),
                egui::TextureOptions::NEAREST,
            )),
            integer_exp: true,
            edit: editor::EditorState::default(),
            export: export::ExportState::default(),
        }
    }
    #[inline(always)]
    fn zcp(&self) -> [Complex64; 3] {
        [self.z.param(), self.c.param(), self.p.param()]
    }
    fn update(&mut self) {
        let zcp = self.zcp();
        self.z.update(FractalPlane::Z, &self.common, zcp);
        self.c.update(FractalPlane::C, &self.common, zcp);
        self.p.update(FractalPlane::P, &self.common, zcp);
        self.z.poll_fut(FractalPlane::Z, &self.common, zcp);
        self.c.poll_fut(FractalPlane::C, &self.common, zcp);
        self.p.poll_fut(FractalPlane::P, &self.common, zcp);
    }
    fn show_plane(&mut self, param: FractalPlane, ctx: &egui::Context) {
        let zcp = self.zcp();
        egui::Window::new(param.plane()).show(ctx, |ui| {
            let (sel, others) = param.select([&mut self.z, &mut self.c, &mut self.p]);
            if sel.show(param, &self.common, zcp, ui) {
                for o in others {
                    o.mark_changed();
                }
            }
        });
    }
}
impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::Window::new("Editor")
            .default_size(egui::vec2(250.0, 250.0))
            .show(ctx, |ui| {
                editor::show(self, ui);
            });
        egui::Window::new("Export").show(ctx, |ui| {
            self.export.show(ui, self.zcp(), &self.common);
        });
        self.update();
        egui::Window::new("Params").show(ctx, |ui| {
            ui.horizontal(|ui| {
                if ui
                    .add(
                        egui::Slider::new(&mut self.common.exponent, -5.0..=5.0)
                            .max_decimals(if self.integer_exp { 0 } else { 2 })
                            .text("Exponent"),
                    )
                    .changed()
                {
                    self.z.mark_changed();
                    self.c.mark_changed();
                    self.p.mark_changed();
                }
                ui.checkbox(&mut self.integer_exp, "Integer");
            });
            if ui
                .add(
                    egui::Slider::new(&mut self.common.depth, 1..=1000)
                        .logarithmic(true)
                        .text("Depth"),
                )
                .changed()
            {
                self.common.upper.regenerate(self.common.depth, false);
                if let Some(lower) = &mut self.common.lower {
                    lower.regenerate(self.common.depth, false);
                }
                self.z.mark_changed();
                self.c.mark_changed();
                self.p.mark_changed();
            }
            show_param(ui, "z", &mut self.z, [&mut self.c, &mut self.p]);
            show_param(ui, "c", &mut self.c, [&mut self.p, &mut self.z]);
            show_param(ui, "P", &mut self.p, [&mut self.z, &mut self.c]);
        });
        self.show_plane(FractalPlane::Z, ctx);
        self.show_plane(FractalPlane::C, ctx);
        self.show_plane(FractalPlane::P, ctx);
        egui::Window::new("Colors").show(ctx, |ui| {
            let renorm_box = ui.checkbox(&mut self.common.renorm, "Renormalize");
            if renorm_box.hovered() {
                renorm_box.show_tooltip_text("Renormalization can help reduce banding in the images, and the results look slightly more vibrant. Try it to see the difference!");
            }
            let mut changed = renorm_box.changed();
            let mut show_lower = self.common.lower.is_some();
            let split_grad_box = ui.checkbox(&mut show_lower, "Split Gradient");
            if split_grad_box.hovered() {
                split_grad_box.show_tooltip_text("Use a linear blend of upper and lower gradients rather than just one");
            }
            changed |= split_grad_box.changed();
            changed |= egui::CollapsingHeader::new(if self.common.lower.is_some() {
                "Upper"
            } else {
                "Gradient"
            })
            .id_salt("Upper")
            .default_open(true)
            .show(ui, |ui| show_picker(ui, &mut self.common.upper))
            .body_returned
            .unwrap_or(false);
            if show_lower {
                let lower = self.common.lower.get_or_insert_with(|| self.common.upper.clone());
                changed |= egui::CollapsingHeader::new("Lower")
                    .default_open(true)
                    .show(ui, |ui| show_picker(ui, lower))
                    .body_returned
                    .unwrap_or(false);
            } else {
                self.common.lower = None;
            }
            if changed {
                self.common.regenerate(true);
                self.z.mark_changed();
                self.c.mark_changed();
                self.p.mark_changed();
            }
        });
        egui::Window::new("Recommended Reading")
            .default_size(egui::vec2(300.0, 400.0))
            .default_open(false)
            .default_pos(egui::pos2(1000.0, 800.0))
            .show(ctx, |ui| {
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
    }
}

fn show_param(
    ui: &mut egui::Ui,
    name: &str,
    plane: &mut plane::ImageData,
    others: [&mut plane::ImageData; 2],
) {
    ui.horizontal(|ui| {
        ui.label(format!("{name}: "));
        ui.vertical(|ui| {
            if ui
                .add(egui::TextEdit::singleline(&mut plane.param_str).desired_width(50.0))
                .changed()
            {
                plane.parse_str();
                if !plane.invalid_param() {
                    for p in others {
                        p.mark_changed();
                    }
                }
            }
            if plane.invalid_param() {
                add_error(&"Invalid parameter", ui, None);
            }
        });
    });
}

fn main() {
    eframe::run_native(
        "Fractal Explorer",
        eframe::NativeOptions {
            centered: true,
            viewport: egui::ViewportBuilder::default().with_inner_size(egui::vec2(1280.0, 900.0)),
            ..Default::default()
        },
        Box::new(|cc| Ok(Box::new(App::new(cc)))),
    )
    .unwrap();
}
