use super::*;

#[derive(Clone)]
pub struct GlState {
    gl: Arc<glow::Context>,
    program: glow::Program,
    vao: glow::VertexArray,
    viewport: glow::UniformLocation,
    depth: glow::UniformLocation,
    exponent: glow::UniformLocation,
    boundary: glow::UniformLocation,
    zcp: glow::UniformLocation,
    param: glow::UniformLocation,
    renorm: glow::UniformLocation,
    upper_exp: glow::UniformLocation,
    num_upper: glow::UniformLocation,
    upper_positions: glow::UniformLocation,
    upper_colors: glow::UniformLocation,
    lower_exp: glow::UniformLocation,
    num_lower: glow::UniformLocation,
    lower_positions: glow::UniformLocation,
    lower_colors: glow::UniformLocation,
}
impl GlState {
    pub fn new(cc: &eframe::CreationContext) -> Result<Self, String> {
        use glow::HasContext;
        unsafe {
            let gl = cc.gl.clone().expect("This app should be run with OpenGL");
            let vertex = gl.create_shader(glow::VERTEX_SHADER)?;
            gl.shader_source(vertex, include_str!("shaders/fractal.vert"));
            gl.compile_shader(vertex);
            if !gl.get_shader_compile_status(vertex) {
                return Err(gl.get_shader_info_log(vertex));
            }

            let fragment = gl.create_shader(glow::FRAGMENT_SHADER)?;
            gl.shader_source(fragment, include_str!("shaders/fractal.frag"));
            gl.compile_shader(fragment);
            if !gl.get_shader_compile_status(fragment) {
                return Err(gl.get_shader_info_log(fragment));
            }

            let program = gl.create_program()?;
            gl.attach_shader(program, vertex);
            gl.attach_shader(program, fragment);
            gl.link_program(program);
            if !gl.get_program_link_status(program) {
                return Err(gl.get_program_info_log(program));
            }

            gl.detach_shader(program, vertex);
            gl.detach_shader(program, fragment);
            gl.delete_shader(vertex);
            gl.delete_shader(fragment);

            let vao = gl.create_vertex_array()?;

            let viewport = gl
                .get_uniform_location(program, "viewport")
                .ok_or("Missing uniform: viewport")?;
            let depth = gl
                .get_uniform_location(program, "depth")
                .ok_or("Missing uniform: depth")?;
            let exponent = gl
                .get_uniform_location(program, "exponent")
                .ok_or("Missing uniform: exponent")?;
            let boundary = gl
                .get_uniform_location(program, "boundary")
                .ok_or("Missing uniform: boundary")?;
            let zcp = gl
                .get_uniform_location(program, "zcp")
                .ok_or("Missing uniform: zcp")?;
            let param = gl
                .get_uniform_location(program, "param")
                .ok_or("Missing uniform: param")?;
            let renorm = gl
                .get_uniform_location(program, "renorm")
                .ok_or("Missing uniform: renorm")?;
            let upper_exp = gl
                .get_uniform_location(program, "upper_exp")
                .ok_or("Missing uniform: upper_exp")?;
            let num_upper = gl
                .get_uniform_location(program, "num_upper")
                .ok_or("Missing uniform: num_upper")?;
            let upper_positions = gl
                .get_uniform_location(program, "upper_positions")
                .ok_or("Missing uniform: upper_positions")?;
            let upper_colors = gl
                .get_uniform_location(program, "upper_colors")
                .ok_or("Missing uniform: upper_colors")?;
            let lower_exp = gl
                .get_uniform_location(program, "lower_exp")
                .ok_or("Missing uniform: lower_exp")?;
            let num_lower = gl
                .get_uniform_location(program, "num_lower")
                .ok_or("Missing uniform: num_upper")?;
            let lower_positions = gl
                .get_uniform_location(program, "lower_positions")
                .ok_or("Missing uniform: lower_positions")?;
            let lower_colors = gl
                .get_uniform_location(program, "lower_colors")
                .ok_or("Missing uniform: lower_colors")?;

            Ok(Self {
                gl,
                program,
                vao,
                viewport,
                depth,
                exponent,
                boundary,
                zcp,
                param,
                renorm,
                upper_exp,
                num_upper,
                upper_positions,
                upper_colors,
                lower_exp,
                num_lower,
                lower_positions,
                lower_colors,
            })
        }
    }
    pub fn gl(&self) -> &Arc<glow::Context> {
        &self.gl
    }
    pub fn render(
        &self,
        common: &CommonData,
        framebuffer: glow::Framebuffer,
        zcp: [Complex32; 3],
        plane: FractalPlane,
        viewport: [i32; 4],
        full_resolution: usize,
    ) {
        let gl = &self.gl;
        unsafe {
            gl.bind_framebuffer(glow::FRAMEBUFFER, Some(framebuffer));
            gl.draw_buffers(&[glow::COLOR_ATTACHMENT0]);
            {
                let [x, y, w, h] = viewport;
                gl.viewport(x, y, w, h);
            }
            gl.use_program(Some(self.program));
            gl.bind_vertex_array(Some(self.vao));

            let mut colors = [[0.0; 4]; 16];
            let mut stops = [0.0; 16];
            let mut len = common.upper.stops.len();
            let mut exp = common.upper.exponential;

            for (((sc, ss), dc), ds) in common.upper.stops.iter().zip(&mut colors).zip(&mut stops) {
                *dc = sc.to_array().map(|v| v as f32 / 255.0);
                *ds = *ss;
            }

            gl.uniform_1_i32(Some(&self.upper_exp), exp as i32);
            gl.uniform_1_i32(Some(&self.num_upper), len as i32);
            gl.uniform_1_f32_slice(Some(&self.upper_positions), &stops);
            gl.uniform_4_f32_slice(Some(&self.upper_colors), colors.as_flattened());

            if let Some(lower) = &common.lower {
                exp = lower.exponential;
                len = lower.stops.len();
                for (((sc, ss), dc), ds) in lower.stops.iter().zip(&mut colors).zip(&mut stops) {
                    *dc = sc.to_array().map(|v| v as f32 / 255.0);
                    *ds = *ss;
                }
            }

            gl.uniform_1_i32(Some(&self.lower_exp), exp as i32);
            gl.uniform_1_i32(Some(&self.num_lower), len as i32);
            gl.uniform_1_f32_slice(Some(&self.lower_positions), &stops);
            gl.uniform_4_f32_slice(Some(&self.lower_colors), colors.as_flattened());

            gl.uniform_1_i32(Some(&self.depth), common.depth as _);
            gl.uniform_1_f32(Some(&self.exponent), common.exponent);
            gl.uniform_1_f32(Some(&self.boundary), common.boundary * common.boundary);
            gl.uniform_2_f32_slice(
                Some(&self.zcp),
                std::slice::from_raw_parts(zcp.as_ptr().cast::<f32>(), 6),
            );
            gl.uniform_1_i32(Some(&self.renorm), common.renorm as _);
            gl.uniform_1_i32(Some(&self.param), plane as u8 as i32);

            {
                let s = 4.0 / (full_resolution as f32);
                let [x, y, w, h] = viewport.map(|v| v as f32 * s);
                gl.uniform_4_f32(Some(&self.viewport), x - 2.0, y - 2.0, w, h);
            }

            gl.draw_arrays(glow::TRIANGLES, 0, 6);

            gl.bind_framebuffer(glow::FRAMEBUFFER, None);
        }
    }
    pub fn texture(&self, resolution: usize) -> Result<glow::Texture, String> {
        unsafe {
            let gl = &self.gl;
            let texture = gl.create_texture()?;
            gl.bind_texture(glow::TEXTURE_2D, Some(texture));
            gl.tex_image_2d(
                glow::TEXTURE_2D,
                0,
                glow::RGBA8 as _,
                resolution as _,
                resolution as _,
                0,
                glow::RGBA,
                glow::UNSIGNED_BYTE,
                glow::PixelUnpackData::Slice(None),
            );
            gl.tex_parameter_i32(
                glow::TEXTURE_2D,
                glow::TEXTURE_MIN_FILTER,
                glow::NEAREST as i32,
            );
            gl.tex_parameter_i32(
                glow::TEXTURE_2D,
                glow::TEXTURE_MAG_FILTER,
                glow::NEAREST as i32,
            );
            // gl.bind_texture(glow::TEXTURE_2D, None);
            Ok(texture)
        }
    }
    pub fn attach_texture(
        &self,
        texture: glow::Texture,
        framebuffer: glow::Framebuffer,
        delete_on_fail: bool,
    ) -> Result<(), String> {
        unsafe {
            let gl = &self.gl;
            gl.bind_texture(glow::TEXTURE_2D, Some(texture));
            gl.bind_framebuffer(glow::FRAMEBUFFER, Some(framebuffer));
            gl.framebuffer_texture(glow::FRAMEBUFFER, glow::COLOR_ATTACHMENT0, Some(texture), 0);
            let status = gl.check_framebuffer_status(glow::FRAMEBUFFER);
            if status != glow::FRAMEBUFFER_COMPLETE {
                if delete_on_fail {
                    gl.delete_texture(texture);
                    gl.delete_framebuffer(framebuffer);
                }
                return Err(format!("Framebuffer incomplete after resize: {status}"));
            }
            gl.bind_framebuffer(glow::FRAMEBUFFER, None);
            gl.bind_texture(glow::TEXTURE_2D, None);
            Ok(())
        }
    }
}
