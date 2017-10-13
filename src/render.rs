use na::{Matrix4, Vector2, Vector4, Diagonal, Isometry3, Vector3, ToHomogeneous};
use glium::{self, Surface, Texture2d};
use glium::backend::Context;
use std::rc::Rc;
use std::ops::Range;
use image;

use rusttype::PositionedGlyph;
use rusttype::gpu_cache::{Cache};

#[derive(Copy, Clone)]
pub struct Vertex {
    pub a_position: [f32; 2],
    pub a_color: [f32; 4],
    pub a_uv: [f32; 2],
}
implement_vertex!(Vertex, a_position, a_color, a_uv);

const NUM_BUFFER_TRIANGLES: usize = 4096;
const VERTICES_PER_TRIANGLE: usize = 3;
const NUM_BUFFER_VERTICES: usize = NUM_BUFFER_TRIANGLES * VERTICES_PER_TRIANGLE;

pub fn load_image(path: &str) -> glium::texture::RawImage2d<u8> {
	let mut i = image::open(path).unwrap().to_rgba();
	let d = i.dimensions();
	for pixel in i.pixels_mut() {
		pixel[0] = (pixel[0] as u16 * pixel[3] as u16 / 255) as u8;
		pixel[1] = (pixel[1] as u16 * pixel[3] as u16 / 255) as u8;
		pixel[2] = (pixel[2] as u16 * pixel[3] as u16 / 255) as u8;
	}
	glium::texture::RawImage2d::from_raw_rgba_reversed(i.into_raw(), d)
}
/// RGBA
pub fn color(c: u32) -> Vector4<f32> {
	fn f(i: u32) -> f32 {
		(i as u8) as f32 / 255.0
	}
	Vector4::new(f(c >> 24), f(c >> 16), f(c >> 8), f(c))
}

pub fn scale(sx: f32, sy: f32, sz: f32) -> Matrix4<f32> {
	Matrix4::from_diagonal(&Vector4::new(sx, sy, sz, 1.0))
}

pub fn translate(x: f32, y: f32, z: f32) -> Matrix4<f32> {
	Isometry3::new(Vector3::new(x, y, z), Vector3::new(0.0, 0.0, 0.0f32)).to_homogeneous()
}

pub fn rotate(a: f32) -> Matrix4<f32> {
	Isometry3::new(Vector3::new(0.0, 0.0, 0.0f32), Vector3::new(0.0, 0.0f32, a)).to_homogeneous()
}

pub struct DynamicRenderer {
	vertex_buffer: glium::VertexBuffer<Vertex>,
	program: glium::Program,
    font_cache_texture: Texture2d,
    font_cache: Cache,
    vertices: Vec<Vertex>, // Used by batch, prevents unnecessary reallocations
}
impl DynamicRenderer {
    pub fn new(facade: &Rc<Context>) -> DynamicRenderer {
        let cache_width = 1024;
        let cache_height = 1024;
        DynamicRenderer {
            vertex_buffer: glium::VertexBuffer::empty_dynamic(facade, NUM_BUFFER_VERTICES).unwrap(),
            program: glium::Program::from_source(facade, VERTEX_SHADER_SRC, FRAGMENT_SHADER_SRC, None).unwrap(),
            // https://github.com/dylanede/rusttype/blob/master/examples/gpu_cache.rs
            font_cache_texture: glium::texture::Texture2d::with_format(
                facade,
                glium::texture::RawImage2d {
                    data: ::std::borrow::Cow::Owned(vec![255u8; cache_width as usize * cache_height as usize]),
                    width: cache_width,
                    height: cache_height,
                    format: glium::texture::ClientFormat::U8
                },
                glium::texture::UncompressedFloatFormat::U8,
                glium::texture::MipmapsOption::NoMipmap).unwrap(),
            font_cache: Cache::new(cache_width, cache_height, 0.1, 0.1),
            vertices: vec![Vertex { a_position: [0.0, 0.0], a_color: [0.0, 0.0, 0.0, 0.0], a_uv: [0.0, 0.0] }; NUM_BUFFER_VERTICES],
        }
    }
    pub fn batch<'a>(&'a mut self, frame: &'a mut glium::Frame, view: &Matrix4<f32>) -> DynamicBatch<'a> {
        DynamicBatch::new(frame, *view, self)
    }
    fn draw_triangle_vertices(&mut self, frame: &mut glium::Frame, view: &Matrix4<f32>, vertex_range: Range<usize>) {
        let vertices = &self.vertices[vertex_range];
        if vertices.len() == 0 { return; }
        let uniforms = uniform! {
            view: view.as_ref().clone(),
            // u_sampler: self.font_texture.sampled().minify_filter(glium::uniforms::MinifySamplerFilter::Nearest).magnify_filter(glium::uniforms::MagnifySamplerFilter::Nearest),
            u_sampler: self.font_cache_texture.sampled().minify_filter(glium::uniforms::MinifySamplerFilter::Nearest).magnify_filter(glium::uniforms::MagnifySamplerFilter::Nearest)
        };
        let params = glium::DrawParameters {
            blend: glium::Blend::alpha_blending(),
            depth: glium::Depth {
                test: glium::DepthTest::IfLessOrEqual,
                write: false,
                .. Default::default()
            },
            .. Default::default()
        };
        let slice = self.vertex_buffer.slice(0..vertices.len()).unwrap();
        slice.write(vertices);
        frame.draw(slice, glium::index::IndicesSource::NoIndices { primitives: glium::index::PrimitiveType::TrianglesList }, &self.program, &uniforms, &params).unwrap();
    }
    pub fn draw_geometry(&self, frame: &mut glium::Frame, view: &Matrix4<f32>, geo: &Geometry) {
        let uniforms = uniform! {
            view: view.as_ref().clone(),
            u_sampler: self.font_cache_texture.sampled().minify_filter(glium::uniforms::MinifySamplerFilter::Nearest).magnify_filter(glium::uniforms::MagnifySamplerFilter::Nearest)
        };
        let params = glium::DrawParameters {
            blend: glium::Blend::alpha_blending(),
            depth: glium::Depth {
                test: glium::DepthTest::IfLessOrEqual,
                write: false,
                .. Default::default()
            },
            .. Default::default()
        };
        frame.draw(&geo.buffer, glium::index::IndicesSource::NoIndices { primitives: glium::index::PrimitiveType::TrianglesList }, &self.program, &uniforms, &params).unwrap();
    }
    pub fn dbg_draw_font_cache(&mut self, frame: &mut glium::Frame) {
        use na;
        let mut batch = self.batch(frame, &na::Eye::new_identity(4));
        let color = self::color(0xff0000ff);
        let vs = [
            Vertex { a_position: [ -1.0, -1.0 ], a_uv: [ 0.0, 0.0 ], a_color: color.as_ref().clone() },
            Vertex { a_position: [  1.0, -1.0 ], a_uv: [ 1.0, 0.0 ], a_color: color.as_ref().clone() },
            Vertex { a_position: [  1.0,  1.0 ], a_uv: [ 1.0, 1.0 ], a_color: color.as_ref().clone() },
            Vertex { a_position: [ -1.0,  1.0 ], a_uv: [ 0.0, 1.0 ], a_color: color.as_ref().clone() },
        ];
        batch.draw_triangle(vs[0], vs[1], vs[2]);
        batch.draw_triangle(vs[0], vs[3], vs[2]);
    }
}

pub struct DynamicBatch<'a> {
    frame: &'a mut glium::Frame,
    view: Matrix4<f32>,
    renderer: &'a mut DynamicRenderer,
    pending_triangles: usize,
}
impl<'a> DynamicBatch<'a> {
    fn new(frame: &'a mut glium::Frame, view: Matrix4<f32>, renderer: &'a mut DynamicRenderer) -> DynamicBatch<'a> {
        DynamicBatch {
            frame: frame,
            view: view,
            renderer: renderer,
            pending_triangles: 0,
        }
    }
    pub fn flush(&mut self) {
        self.renderer.draw_triangle_vertices(self.frame, &self.view, 0..self.pending_triangles * VERTICES_PER_TRIANGLE);
        self.pending_triangles = 0;
    }
    pub fn draw_vertices(&mut self, vertices: &[Vertex]) {
        for chunk in vertices.chunks(3) {
            self.draw_triangle(chunk[0], chunk[1], chunk[2]);
        }
    }
}
impl<'a> GeometryGeneratorCache for DynamicBatch<'a> {
    fn get_font_cache(&mut self) -> (&mut Cache, &Texture2d) { (&mut self.renderer.font_cache, &self.renderer.font_cache_texture) }
}
impl<'a> GeometryGenerator for DynamicBatch<'a> {
    fn draw_triangle(&mut self, a: Vertex, b: Vertex, c: Vertex) {
        if self.pending_triangles >= NUM_BUFFER_TRIANGLES {
            self.flush();
        }
        let i = self.pending_triangles * VERTICES_PER_TRIANGLE;
        self.renderer.vertices[i + 0] = a;
        self.renderer.vertices[i + 1] = b;
        self.renderer.vertices[i + 2] = c;
        self.pending_triangles += 1;
    }
}
impl<'a> Drop for DynamicBatch<'a> {
    fn drop(&mut self) {
        self.flush();
    }
}

const VERTEX_SHADER_SRC: &'static str = r#"
    #version 140
	uniform mat4 view;
    in vec2 a_position;
	in vec4 a_color;
    in vec2 a_uv;
	out vec4 v_color;
    out vec2 v_uv;
    void main() {
        gl_Position = view * vec4(a_position, 0.0, 1.0);
		v_color = a_color;
        v_uv = a_uv;
    }
"#;

const FRAGMENT_SHADER_SRC: &'static str = r#"
    #version 140
    uniform sampler2D u_sampler;
	in vec4 v_color;
    in vec2 v_uv;
    out vec4 color;
    void main() {
        float alpha = texture(u_sampler, v_uv).r;
        alpha = 1.0 - pow(1.0 - alpha, 2.5);
        color = vec4(v_color.rgb, v_color.a * alpha);
    }
"#;

trait GeometryGeneratorCache {
    fn get_font_cache(&mut self) -> (&mut Cache, &Texture2d);
}

pub trait GeometryGenerator: GeometryGeneratorCache {
    fn draw_triangle(&mut self, a: Vertex, b: Vertex, c: Vertex);
    fn draw_quad(&mut self, p: Vector2<f32>, size: Vector2<f32>, color: Vector4<f32>) {
        let verts = [
            Vertex { a_position: [ -1.0, -1.0], a_uv: [1.0, 1.0], a_color: color.as_ref().clone() },
            Vertex { a_position: [  1.0, -1.0], a_uv: [1.0, 1.0], a_color: color.as_ref().clone() },
            Vertex { a_position: [  1.0,  1.0], a_uv: [1.0, 1.0], a_color: color.as_ref().clone() },
            Vertex { a_position: [ -1.0,  1.0], a_uv: [1.0, 1.0], a_color: color.as_ref().clone() }
        ];
        let vs: Vec<_> = verts.iter().map(move |vert| {
            let x = p.x + vert.a_position[0] * size.x * 0.5;
            let y = p.y + vert.a_position[1] * size.y * 0.5;
            Vertex { a_position: [x, y], .. *vert }
        }).collect();
        self.draw_triangle(vs[0], vs[1], vs[2]);
        self.draw_triangle(vs[0], vs[3], vs[2]);
    }
    fn draw_glyphs(&mut self, p: Vector2<f32>, glyphs: &[PositionedGlyph<'static>], color: Vector4<f32>) {
        for glyph in glyphs {
            self.get_font_cache().0.queue_glyph(0, glyph.clone());
        }

        {
            let (font_cache, font_cache_texture) = self.get_font_cache();
            //https://github.com/dylanede/rusttype/blob/master/examples/gpu_cache.rs
            font_cache.cache_queued(|rect, data| {
                font_cache_texture.main_level().write(glium::Rect {
                    left: rect.min.x,
                    bottom: rect.min.y,
                    width: rect.width(),
                    height: rect.height()
                }, glium::texture::RawImage2d {
                    data: ::std::borrow::Cow::Borrowed(data),
                    width: rect.width(),
                    height: rect.height(),
                    format: glium::texture::ClientFormat::U8
                });
            }).unwrap();
        }

        for glyph in glyphs {
            if let Ok(Some((uv, world))) = self.get_font_cache().0.rect_for(0, glyph) {
                let vs = [
                    Vertex { a_position: [ p.x + world.min.x as f32, p.y - world.min.y as f32 ], a_uv: [ uv.min.x, uv.min.y ], a_color: color.as_ref().clone() },
                    Vertex { a_position: [ p.x + world.max.x as f32, p.y - world.min.y as f32 ], a_uv: [ uv.max.x, uv.min.y ], a_color: color.as_ref().clone() },
                    Vertex { a_position: [ p.x + world.max.x as f32, p.y - world.max.y as f32 ], a_uv: [ uv.max.x, uv.max.y ], a_color: color.as_ref().clone() },
                    Vertex { a_position: [ p.x + world.min.x as f32, p.y - world.max.y as f32 ], a_uv: [ uv.min.x, uv.max.y ], a_color: color.as_ref().clone() },
                ];
                self.draw_triangle(vs[0], vs[1], vs[2]);
                self.draw_triangle(vs[0], vs[3], vs[2]);
            }
        }
    }
}

pub struct GeometryBuilder<'a> {
    vertices: Vec<Vertex>,
    renderer: &'a mut DynamicRenderer,
}
impl<'a> GeometryBuilder<'a> {
    pub fn new(renderer: &'a mut DynamicRenderer) -> GeometryBuilder<'a> {
        GeometryBuilder {
            vertices: vec![],
            renderer: renderer,
        }
    }
    pub fn build(&self, context: &Rc<Context>) -> Geometry {
        Geometry { buffer: glium::VertexBuffer::new(context, &self.vertices).unwrap() }
    }
    pub fn into_vertices(self) -> Vec<Vertex> {
        self.vertices
    }
}
impl<'a> GeometryGeneratorCache for GeometryBuilder<'a> {
    fn get_font_cache(&mut self) -> (&mut Cache, &Texture2d) { (&mut self.renderer.font_cache, &self.renderer.font_cache_texture) }
}
impl<'a> GeometryGenerator for GeometryBuilder<'a> {
    fn draw_triangle(&mut self, a: Vertex, b: Vertex, c: Vertex) {
        self.vertices.push(a);
        self.vertices.push(b);
        self.vertices.push(c);
    }
}

pub struct Geometry {
    buffer: glium::VertexBuffer<Vertex>
}