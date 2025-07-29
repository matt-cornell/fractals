use egui::epaint::Hsva;

use super::*;
use std::fmt::Write;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll, Waker};

enum TextureKind {
    Glow(glow::Texture),
    Egui(egui::TextureId),
}
impl TextureKind {
    fn resolve(&mut self, frame: &mut eframe::Frame) -> egui::TextureId {
        match self {
            Self::Glow(tex) => {
                let tex = frame.register_native_glow_texture(*tex);
                *self = Self::Egui(tex);
                tex
            }
            Self::Egui(tex) => *tex,
        }
    }
}

pub struct ImageData {
    resolution: usize,
    changed: bool,
    param: Complex32,
    pub param_str: String,
    invalid_param: bool,
    save_fut: Option<(Pin<Box<dyn Future<Output = Option<rfd::FileHandle>>>>, bool)>,
    save_res: Result<(), String>,
    marker: bool,
    realloc: Option<usize>,
    framebuffer: glow::Framebuffer,
    texture: TextureKind,
}
impl ImageData {
    pub fn new(gl: &gl::GlState) -> Result<Self, String> {
        let texture = gl.texture(256)?;
        let framebuffer;
        unsafe {
            framebuffer = gl.gl().create_framebuffer()?;
            gl.attach_texture(texture, framebuffer, true)?;
        }
        Ok(Self {
            resolution: 256,
            changed: true,
            param: Complex32::ZERO,
            param_str: Complex32::ZERO.to_string(),
            invalid_param: false,
            save_fut: None,
            save_res: Ok(()),
            marker: true,
            realloc: None,
            framebuffer,
            texture: TextureKind::Glow(texture),
        })
    }
    pub fn parse_str(&mut self) {
        if let Ok(p) = self.param_str.parse() {
            self.param = p;
            self.invalid_param = false;
        } else {
            self.invalid_param = true;
        }
    }
    #[inline(always)]
    pub fn param(&self) -> Complex32 {
        self.param
    }
    #[inline(always)]
    pub fn mark_changed(&mut self) {
        self.changed = true;
    }
    #[inline(always)]
    pub fn changed(&self) -> bool {
        self.changed || self.realloc.is_some()
    }
    #[inline(always)]
    pub fn invalid_param(&self) -> bool {
        self.invalid_param
    }
    fn frame_data(&self, gl: &Arc<glow::Context>) -> egui::ColorImage {
        unsafe {
            let res = self.realloc.unwrap_or(self.resolution);
            let mut buffer = vec![Color32::PLACEHOLDER; res * res];
            gl.bind_framebuffer(glow::FRAMEBUFFER, Some(self.framebuffer));
            gl.read_buffer(glow::COLOR_ATTACHMENT0);
            gl.read_pixels(
                0,
                0,
                res as _,
                res as _,
                glow::RGBA,
                glow::UNSIGNED_BYTE,
                glow::PixelPackData::Slice(Some(bytemuck::cast_slice_mut(&mut buffer))),
            );
            gl.bind_framebuffer(glow::FRAMEBUFFER, None);
            egui::ColorImage {
                size: [self.resolution; 2],
                source_size: egui::Vec2::splat(self.resolution as f32),
                pixels: buffer,
            }
        }
    }
    pub fn update(
        &mut self,
        plane: FractalPlane,
        gl: &gl::GlState,
        common: &CommonData,
        zcp: [Complex32; 3],
        ctx: &egui::Context,
    ) {
        if let Some(old) = self.realloc.take() {
            if let Ok(texture) = gl.texture(self.resolution) {
                if gl.attach_texture(texture, self.framebuffer, false).is_ok() {
                    unsafe {
                        match self.texture {
                            TextureKind::Glow(tex) => gl.gl().delete_texture(tex),
                            TextureKind::Egui(tex) => ctx.tex_manager().write().free(tex),
                        }
                    }
                    self.texture = TextureKind::Glow(texture);
                    self.changed = true;
                } else {
                    unsafe {
                        gl.gl().delete_texture(texture);
                    }
                }
            } else {
                self.resolution = old;
            }
        }
        if !self.changed {
            return;
        }
        self.changed = false;
        gl.render(
            common,
            self.framebuffer,
            zcp,
            plane,
            [0, 0, self.resolution as _, self.resolution as i32],
            self.resolution,
        );
    }
    pub fn show(
        &mut self,
        plane: FractalPlane,
        ui: &mut egui::Ui,
        frame: &mut eframe::Frame,
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
            if ui.button("Zero").clicked() {
                self.param = Complex32::ZERO;
                self.param_str.clear();
                let _ = write!(self.param_str, "{}", self.param);
                self.mark_changed();
                updated = true;
            }
            if ui.checkbox(&mut self.marker, "Show marker").changed() {
                self.mark_changed();
            }
        });
        let old = self.resolution;
        let new_res = ui
            .add(
                egui::Slider::new(&mut self.resolution, 0..=4096)
                    .clamping(egui::SliderClamping::Never)
                    .logarithmic(true)
                    .text("Resolution"),
            )
            .changed();
        if new_res {
            self.realloc = self.realloc.or(Some(old));
            self.mark_changed();
        }
        if let Err(err) = &self.save_res {
            let mut clicked = false;
            add_error(err, ui, Some(&mut || clicked = true));
            if clicked {
                self.save_res = Ok(());
            }
        }
        let mut img = egui::ScrollArea::both()
            .show(ui, |ui| {
                ui.image(egui::load::SizedTexture::new(
                    self.texture.resolve(frame),
                    [self.resolution as f32; 2],
                ))
                .interact(egui::Sense::click())
            })
            .inner;
        if self.marker {
            img = img.interact(egui::Sense::click_and_drag());
            let scale = 4.0 / (self.resolution as f32);
            if img.clicked_by(egui::PointerButton::Primary)
                || img.dragged_by(egui::PointerButton::Primary)
            {
                if let Some(pos) = img.interact_pointer_pos() {
                    let p = pos - img.rect.min;
                    let x = p.x * scale - 2.0;
                    let y = p.y * scale - 2.0;
                    let res = self.resolution as f32;
                    self.param = Complex32::new((x * res).round() / res, (-y * res).round() / res);
                    self.param_str.clear();
                    let _ = write!(self.param_str, "{}", self.param);
                    self.mark_changed();
                    updated = true;
                }
            }
            let (tx, ty) = (
                ((self.param.re + 2.0) / scale) as usize,
                ((-self.param.im + 2.0) / scale) as usize,
            );
            let mut px = Color32::PLACEHOLDER;
            unsafe {
                let gl = frame.gl().unwrap();
                gl.bind_framebuffer(glow::FRAMEBUFFER, Some(self.framebuffer));
                gl.read_buffer(glow::COLOR_ATTACHMENT0);
                gl.read_pixels(
                    tx as _,
                    ty as _,
                    1,
                    1,
                    glow::RGBA,
                    glow::UNSIGNED_BYTE,
                    glow::PixelPackData::Slice(Some(bytemuck::bytes_of_mut(&mut px))),
                );
                gl.bind_framebuffer(glow::FRAMEBUFFER, None);
            }
            let mut hsv = Hsva::from(px);
            hsv.s = 1.0;
            hsv.h += 0.5;
            hsv.h %= 1.0;
            hsv.v = (1.0 - hsv.v).max(0.5);
            let color = Color32::from(hsv);
            let painter = ui.painter_at(img.rect);
            painter.vline(
                tx as f32 + img.rect.min.x,
                (ty as f32 - 5.0 + img.rect.min.y)..=(ty as f32 + 5.0 + img.rect.min.y),
                (1.0, color),
            );
            painter.hline(
                (tx as f32 - 5.0 + img.rect.min.x)..=(tx as f32 + 5.0 + img.rect.min.x),
                ty as f32 + img.rect.min.y,
                (1.0, color),
            );
        }
        img.context_menu(|ui| {
            if ui.button("Copy").clicked() {
                ui.ctx().copy_image(self.frame_data(frame.gl().unwrap()));
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
                let mut img = self.frame_data(frame.gl().unwrap());
                add_marker(self.param, self.resolution, &mut img.pixels);
                ui.ctx().copy_image(img);
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
    pub fn poll_fut(&mut self, gl: &Arc<glow::Context>) -> bool {
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
                    let mut buffer = self.frame_data(gl).pixels;
                    if marker {
                        add_marker(self.param, self.resolution, &mut buffer);
                    }
                    if let Err(err) = writer.write_image_data(bytemuck::cast_slice(&buffer)) {
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

fn add_marker(target: Complex32, resolution: usize, buffer: &mut Vec<Color32>) {
    let scale = 4.0 / (resolution as f32);
    let (tx, ty) = (
        ((target.re + 2.0) / scale) as usize,
        ((-target.im + 2.0) / scale) as usize,
    );
    let px = buffer[ty * resolution + tx];
    let mut hsv = Hsva::from(px);
    hsv.s = 1.0;
    hsv.h += 0.5;
    hsv.h %= 1.0;
    hsv.v = (1.0 - hsv.v).max(0.5);
    let color = Color32::from(hsv);
    let xmin = tx.saturating_sub(5);
    let xmax = tx.saturating_add(5).min(resolution - 1);
    let ymin = ty.saturating_sub(5);
    let ymax = ty.saturating_add(5).min(resolution - 1);
    buffer[(ty * resolution + xmin)..=(ty * resolution + xmax)].fill(color);
    for y in ymin..=ymax {
        buffer[y * resolution + tx] = color;
    }
}
