use super::*;
use std::fmt::Write;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll, Waker};

pub struct ImageData {
    resolution: usize,
    buffer: Vec<Color32>,
    handle: egui::TextureHandle,
    changed: bool,
    param: Complex64,
    pub param_str: String,
    invalid_param: bool,
    save_fut: Option<(Pin<Box<dyn Future<Output = Option<rfd::FileHandle>>>>, bool)>,
    save_res: Result<(), String>,
    marker: bool,
}
impl ImageData {
    pub fn new(handle: egui::TextureHandle) -> Self {
        Self {
            handle,
            resolution: 256,
            buffer: Vec::new(),
            changed: true,
            param: Complex64::ZERO,
            param_str: Complex64::ZERO.to_string(),
            invalid_param: false,
            save_fut: None,
            save_res: Ok(()),
            marker: true,
        }
    }
    pub fn parse_str(&mut self) {
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
    pub fn param(&self) -> Complex64 {
        self.param
    }
    #[inline(always)]
    pub fn mark_changed(&mut self) {
        self.changed = true;
    }
    #[inline(always)]
    pub fn changed(&self) -> bool {
        self.changed
    }
    #[inline(always)]
    pub fn invalid_param(&self) -> bool {
        self.invalid_param
    }
    pub fn update(&mut self, plane: FractalPlane, common: &CommonData, zcp: [Complex64; 3]) {
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
        self.handle.set(img, egui::TextureOptions::NEAREST);
    }
    pub fn show(
        &mut self,
        plane: FractalPlane,
        common: &CommonData,
        zcp: [Complex64; 3],
        ui: &mut egui::Ui,
    ) -> bool {
        let mut updated = false;
        ui.horizontal(|ui| {
            ui.label(format!("{}: ", plane.name()));
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
        let mut img = ui.image(&self.handle).interact(egui::Sense::drag());
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
    pub fn poll_fut(
        &mut self,
        plane: FractalPlane,
        common: &CommonData,
        zcp: [Complex64; 3],
    ) -> bool {
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
                false
            } else {
                true
            }
        } else {
            false
        }
    }
}

pub fn render_fractal(
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
