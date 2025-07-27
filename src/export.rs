use super::*;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll, Waker};

enum Progress {
    Ready,
    Rendering {
        plane: FractalPlane,
        zcp: [Complex32; 3],
        res: i32,
        x: i32,
        y: i32,
        common: CommonData,
        frame: glow::Framebuffer,
        texture: glow::Texture,
    },
    Waiting {
        fut: Pin<Box<dyn Future<Output = Option<rfd::FileHandle>>>>,
        res: usize,
        frame: glow::Framebuffer,
        texture: glow::Texture,
    },
}

pub struct ExportState {
    export_sel: FractalPlane,
    export_size: usize,
    export_res: Result<(), String>,
    progress: Progress,
}
impl Default for ExportState {
    fn default() -> Self {
        Self {
            export_sel: FractalPlane::Z,
            export_size: 8192,
            export_res: Ok(()),
            progress: Progress::Ready,
        }
    }
}
impl ExportState {
    pub fn show(
        &mut self,
        ui: &mut egui::Ui,
        zcp: [Complex32; 3],
        common: &CommonData,
        gl: &gl::GlState,
    ) {
        egui::ComboBox::new("Fractal Plane", "Fractal Plane")
            .selected_text(match self.export_sel {
                FractalPlane::Z => "z",
                FractalPlane::C => "c",
                FractalPlane::P => "P",
            })
            .show_ui(ui, |ui| {
                ui.selectable_value(&mut self.export_sel, FractalPlane::Z, "z");
                ui.selectable_value(&mut self.export_sel, FractalPlane::C, "c");
                ui.selectable_value(&mut self.export_sel, FractalPlane::P, "P");
            });
        ui.add_enabled(
            matches!(self.progress, Progress::Ready),
            egui::Slider::new(&mut self.export_size, 0..=10000)
                .clamping(egui::SliderClamping::Never)
                .logarithmic(true),
        );
        self.export_size = self.export_size.min(i32::MAX as _);
        match self.progress {
            Progress::Ready => {
                if ui.button("Export").clicked() {
                    self.export_res = Ok(());
                    unsafe {
                        let texture = match gl.texture(self.export_size) {
                            Ok(tex) => tex,
                            Err(err) => {
                                self.export_res = Err(format!("Failed to create texture: {err}"));
                                return;
                            }
                        };
                        let framebuffer = match gl.gl().create_framebuffer() {
                            Ok(buf) => buf,
                            Err(err) => {
                                gl.gl().delete_texture(texture);
                                self.export_res =
                                    Err(format!("Failed to create framebuffer: {err}"));
                                return;
                            }
                        };
                        self.export_res = gl.attach_texture(texture, framebuffer, true);
                        self.progress = Progress::Rendering {
                            plane: self.export_sel,
                            zcp,
                            res: self.export_size as _,
                            x: 0,
                            y: 0,
                            common: common.clone(),
                            frame: framebuffer,
                            texture,
                        };
                    }
                }
            }
            Progress::Rendering {
                plane,
                zcp,
                res,
                ref mut x,
                ref mut y,
                ref common,
                frame,
                texture,
            } => {
                let num_rows = (res as usize).div_ceil(512);
                let progress = ((*y / 512) as usize * num_rows + (*x / 512) as usize) as f32
                    / (num_rows * num_rows) as f32;
                let mut cancel = false;
                ui.horizontal(|ui| {
                    ui.spinner();
                    ui.label(format!("Processing {:.1}%", progress * 100.0));
                    if ui.button("Cancel").clicked() {
                        cancel = true;
                    }
                });
                ui.add(egui::ProgressBar::new(progress));
                if cancel {
                    unsafe {
                        gl.gl().delete_framebuffer(frame);
                        gl.gl().delete_texture(texture);
                    }
                    self.progress = Progress::Ready;
                    return;
                }
                gl.render(
                    common,
                    frame,
                    zcp,
                    plane,
                    [*x, *y, (res - *x).min(512), (res - *y).min(512)],
                    res as _,
                );
                *x += 512;
                if *x >= res {
                    *x = 0;
                    *y += 512;
                }
                if *y >= res {
                    let fut = rfd::AsyncFileDialog::new()
                        .add_filter("Images", &["png"])
                        .set_file_name("fractal.png")
                        .save_file();
                    self.progress = Progress::Waiting {
                        fut: Box::pin(fut),
                        res: res as _,
                        frame,
                        texture,
                    };
                }
            }
            Progress::Waiting {
                ref mut fut,
                res,
                frame,
                texture,
            } => {
                let mut cancel = false;
                ui.horizontal(|ui| {
                    ui.label("Ready to save");
                    if ui.button("Cancel").clicked() {
                        cancel = true;
                    }
                });
                if cancel {
                    self.progress = Progress::Ready;
                    unsafe {
                        gl.gl().delete_framebuffer(frame);
                        gl.gl().delete_texture(texture);
                    }
                    return;
                }
                let waker = Waker::noop();
                let mut cx = Context::from_waker(waker);
                let Poll::Ready(handle) = fut.as_mut().poll(&mut cx) else {
                    ui.ctx().request_repaint_after(Duration::from_millis(100));
                    return;
                };
                let Some(handle) = handle else {
                    self.progress = Progress::Ready;
                    unsafe {
                        gl.gl().delete_framebuffer(frame);
                    }
                    return;
                };

                'save: {
                    let Ok(file) = std::fs::File::create(handle.path()).inspect_err(|err| {
                        self.export_res = Err(format!("Failed to open file: {err}"))
                    }) else {
                        break 'save;
                    };
                    let mut encoder = png::Encoder::new(file, res as _, res as _);
                    encoder.set_color(png::ColorType::Rgba);
                    let Ok(mut writer) = encoder.write_header().inspect_err(|err| {
                        self.export_res = Err(format!("Failed to save image: {err}"))
                    }) else {
                        break 'save;
                    };
                    let mut buffer = vec![0; res * res * 4];
                    unsafe {
                        let gl = gl.gl();
                        gl.bind_framebuffer(glow::FRAMEBUFFER, Some(frame));
                        gl.read_buffer(glow::COLOR_ATTACHMENT0);
                        gl.read_pixels(
                            0,
                            0,
                            res as _,
                            res as _,
                            glow::RGBA,
                            glow::UNSIGNED_BYTE,
                            glow::PixelPackData::Slice(Some(&mut buffer)),
                        );
                        gl.bind_framebuffer(glow::FRAMEBUFFER, None);
                    }
                    if let Err(err) = writer.write_image_data(&buffer) {
                        self.export_res = Err(format!("Failed to save image: {err}"));
                        break 'save;
                    }
                    if let Err(err) = writer.finish() {
                        self.export_res = Err(format!("Failed to save image: {err}"));
                        break 'save;
                    }
                }

                self.progress = Progress::Ready;
                unsafe {
                    gl.gl().delete_framebuffer(frame);
                    gl.gl().delete_texture(texture);
                }
            }
        }
        if let Err(err) = &self.export_res {
            let mut clicked = false;
            add_error(err, ui, Some(&mut || clicked = true));
            if clicked {
                self.export_res = Ok(());
            }
        }
    }
}
