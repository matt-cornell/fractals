#version 330 core
out vec2 frag_coord;

uniform vec4 viewport;

const vec2 positions[6] = vec2[](
  vec2(0.0, 0.0),
  vec2(1.0, 0.0),
  vec2(1.0, 1.0),
  vec2(0.0, 0.0),
  vec2(1.0, 1.0),
  vec2(0.0, 1.0)
);

void main() {
  vec2 pos = positions[gl_VertexID];
  frag_coord = vec2(pos.x * viewport.z + viewport.x, pos.y * viewport.w + viewport.y);
  gl_Position = vec4(pos * 2.0 - vec2(1.0), 0.0, 1.0);
}
