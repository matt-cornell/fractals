#version 330 core
in vec2 frag_coord;
out vec4 frag_color;

uniform int depth;
uniform float exponent;
uniform float boundary;
uniform vec2[3] zcp;

uniform int param;

uniform bool renorm;

uniform bool upper_exp;
uniform int num_upper;
uniform float upper_positions[16];
uniform vec4 upper_colors[16];

uniform bool lower_exp;
uniform int num_lower;
uniform float lower_positions[16];
uniform vec4 lower_colors[16];

const float LN_2 = 0.693147181;
const float EXP_BASE = 1.1;

vec4 upper_color(float t) {
  if (num_upper == 0 || num_upper >= 16) return vec4(0.0);

  if (upper_exp) t = 1.0 - pow(EXP_BASE, -t);
  else t /= depth;

  if (t < upper_positions[0]) return upper_colors[0];
  for (int i = 0; i < num_upper - 1; ++i) {
    float next = upper_positions[i + 1];
    if (t <= next) {
      float prev = upper_positions[i];
      float mapped = (t - prev) / (next - prev);
      return mix(upper_colors[i], upper_colors[i + 1], mapped);
    }
  }
  return upper_colors[num_upper - 1];
}

vec4 lower_color(float t) {
  if (num_lower == 0 || num_lower >= 16) return vec4(0.0);

  if (lower_exp) t = 1.0 - pow(EXP_BASE, -t);
  else t /= depth;

  if (t < lower_positions[0]) return lower_colors[0];
  for (int i = 0; i < num_lower - 1; ++i) {
    float next = lower_positions[i + 1];
    if (t <= next) {
      float prev = lower_positions[i];
      float mapped = (t - prev) / (next - prev);
      return mix(lower_colors[i], lower_colors[i + 1], mapped);
    }
  }
  return lower_colors[num_lower - 1];
}

vec4 color(int steps, vec2 final) {
  float d = float(steps);
  if (renorm) {
    d += 1.0;
    d -= log(log(length(final))) / LN_2;
  }
  return mix(upper_color(d), lower_color(d), frag_coord.y * 0.25 + 0.5);
}

vec2 cmul(vec2 a, vec2 b) {
  return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}
vec2 cpow(vec2 a, float b) {
  if (b == 2.0) return cmul(a, a);
  if (b == 1.0) return a;
  if (b == 0.0) return vec2(1.0, 0.0);
  float r = length(a);
  float t = atan(a.y, a.x);
  float rb = pow(r, b);
  float tb = t * b;
  return vec2(rb * cos(tb), rb * sin(tb));
}

void main() {
  vec2 old = vec2(0, 0);
  vec2 z = param == 0 ? frag_coord : zcp[0];
  vec2 c = param == 1 ? frag_coord : zcp[1];
  vec2 p = param == 2 ? frag_coord : zcp[2];
  int s = 0;
  for (; s < depth; ++s) {
    vec2 o = z;
    z = cpow(z, exponent) + c + cmul(p, old);
    old = o;
    if (dot(z, z) > boundary) break;
  }
  frag_color = color(s, z);
}
