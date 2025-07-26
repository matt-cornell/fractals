use super::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

const SPECIAL_BASE: usize = usize::MAX >> 2;
const READY: usize = SPECIAL_BASE;
const WAITING: usize = SPECIAL_BASE + 1;
const CANCELLING: usize = SPECIAL_BASE >> 1;

pub struct ExportState {
    export_sel: FractalPlane,
    export_size: usize,
    export_state: Arc<AtomicUsize>,
    export_res: Arc<Mutex<Result<(), String>>>,
}
impl Default for ExportState {
    fn default() -> Self {
        Self {
            export_sel: FractalPlane::Z,
            export_size: 8192,
            export_state: Arc::new(AtomicUsize::new(READY)),
            export_res: Arc::new(Mutex::new(Ok(()))),
        }
    }
}
impl ExportState {
    pub fn show(&mut self, ui: &mut egui::Ui, zcp: [Complex64; 3], common: &CommonData) {
        let state = self.export_state.load(Ordering::Acquire);
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
            state == READY,
            egui::Slider::new(&mut self.export_size, 0..=10000)
                .clamping(egui::SliderClamping::Never)
                .logarithmic(true),
        );

        match state {
            READY => {
                if ui.button("Export").clicked() {
                    *self.export_res.lock().unwrap() = Ok(());
                    let state = Arc::clone(&self.export_state);
                    let common = common.clone();
                    let res = Arc::clone(&self.export_res);
                    let export_sel = self.export_sel;
                    let export_size = self.export_size;
                    std::thread::spawn(move || {
                        state.store(0, Ordering::Release);
                        let mut buffer = vec![Color32::PLACEHOLDER; export_size * export_size];
                        let broke = plane::render_fractal(
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
                        let handle = pollster::block_on(fut);
                        let Some(handle) = handle else {
                            state.store(READY, Ordering::Release);
                            return;
                        };

                        'save: {
                            let Ok(file) =
                                std::fs::File::create(handle.path()).inspect_err(|err| {
                                    *res.lock().unwrap() =
                                        Err(format!("Failed to open file: {err}"))
                                })
                            else {
                                break 'save;
                            };
                            let mut encoder =
                                png::Encoder::new(file, export_size as _, export_size as _);
                            encoder.set_color(png::ColorType::Rgba);
                            let Ok(mut writer) = encoder.write_header().inspect_err(|err| {
                                *res.lock().unwrap() = Err(format!("Failed to save image: {err}"))
                            }) else {
                                break 'save;
                            };
                            if let Err(err) = writer.write_image_data(bytemuck::cast_slice(&buffer))
                            {
                                *res.lock().unwrap() = Err(format!("Failed to save image: {err}"));
                                break 'save;
                            }
                            if let Err(err) = writer.finish() {
                                *res.lock().unwrap() = Err(format!("Failed to save image: {err}"));
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
                        self.export_state.store(CANCELLING, Ordering::Release);
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
                let progress = step as f32 / (self.export_size * self.export_size) as f32;
                ui.horizontal(|ui| {
                    ui.spinner();
                    ui.label(format!(
                        "Processing {}/{} ({:.1}%)",
                        step,
                        self.export_size * self.export_size,
                        progress * 100.0
                    ));
                    if ui.button("Cancel").clicked() {
                        self.export_state.store(CANCELLING, Ordering::Release);
                    }
                });
                ui.add(egui::ProgressBar::new(progress));
            }
        }
        let mut guard = self.export_res.lock().unwrap();
        if let Err(err) = &*guard {
            let mut clicked = false;
            add_error(err, ui, Some(&mut || clicked = true));
            if clicked {
                *guard = Ok(());
            }
        }
    }
}
