#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <GLES2/gl2.h>
#include <string.h>
#include <stdio.h>

typedef struct {
  GLuint program, viewsize, viewxform, tex, frag, vert_vbo;
  int valid;
} gl_state;

static const char *source_vertex_shader =
"uniform vec2 viewSize;\n"
"uniform vec3 viewXform[3];\n"
"attribute vec2 vertex;\n"
"attribute vec2 tcoord;\n"
"varying vec2 ftcoord;\n"
"varying vec2 fpos;\n"
"\n"
"void main(void) {\n"
"  fpos = (mat3(viewXform[0], viewXform[1], viewXform[2]) * vec3(vertex,1.0)).xy;\n"
"  if (tcoord.x < -1.0) {\n"
"  \n"
"    ftcoord = - floor(tcoord) / 2.0 - 1.0;\n"
"    vec2 d = (fract(tcoord) - 0.5) * 8.0;\n"
"    if (length(d) > 0.0001)\n"
"      fpos += normalize(mat2(viewXform[0].xy, viewXform[1].xy) * d) * length(d);\n"
"  }\n"
"  else\n"
"  {\n"
"    ftcoord = tcoord;\n"
"  };\n"
"  gl_Position = vec4(2.0 * fpos.x / viewSize.x - 1.0,\n"
"                     1.0 - 2.0 * fpos.y / viewSize.y, 0, 1);\n"
"}\n";

static const char *source_fragment_shader =
"#define innerCol     frag[6]\n"
"#define outerCol     frag[7]\n"
"#define paintMat     mat3(frag[3].xyz, frag[4].xyz, frag[5].xyz)\n"
"#define scissorMat   mat3(frag[0].xyz, frag[1].xyz, frag[2].xyz)\n"
"#define scissorExt   frag[8].xy\n"
"#define scissorScale frag[8].zw\n"
"#define extent       frag[9].xy\n"
"#define radius       frag[9].z\n"
"#define feather      frag[9].w\n"
"#define strokeMult   frag[10].x\n"
"#define strokeThr    frag[10].y\n"
"#define texType      int(frag[10].z)\n"
"#define type         int(frag[10].w)\n"
"\n"
"uniform vec4 frag[11];\n"
"uniform sampler2D tex;\n"
"varying vec2 ftcoord;\n"
"varying vec2 fpos;\n"
"\n"
"float sdroundrect(vec2 pt, vec2 ext, float rad) {\n"
"  vec2 ext2 = ext - vec2(rad,rad);\n"
"  vec2 d = abs(pt) - ext2;\n"
"  return min(max(d.x,d.y),0.0) + length(max(d,0.0)) - rad;\n"
"}\n"
"\n"
"// Scissoring\n"
"float scissorMask(vec2 p) {\n"
"  vec2 sc = (abs((scissorMat * vec3(p,1.0)).xy) - scissorExt);\n"
"  sc = vec2(0.5,0.5) - sc * scissorScale;\n"
"  return clamp(sc.x,0.0,1.0) * clamp(sc.y,0.0,1.0);\n"
"}\n"
"#ifdef EDGE_AA\n"
"// Stroke - from [0..1] to clipped pyramid, where the slope is 1px.\n"
"float strokeMask() {\n"
"  return min(1.0, (1.0-abs(ftcoord.x*2.0-1.0))*strokeMult) * min(1.0, ftcoord.y);\n"
"}\n"
"#endif\n"
"\n"
"void main(void) {\n"
"   vec4 result;\n"
"  float scissor = scissorMask(fpos);\n"
"#ifdef EDGE_AA\n"
"  float strokeAlpha = strokeMask();\n"
"  if (strokeAlpha < strokeThr) discard;\n"
"#else\n"
"  float strokeAlpha = 1.0;\n"
"#endif\n"
"  if (type == 0) {      // Gradient\n"
"    // Calculate gradient color using box gradient\n"
"    vec2 pt = (paintMat * vec3(fpos,1.0)).xy;\n"
"    float d = clamp((sdroundrect(pt, extent, radius) + feather*0.5) / feather, 0.0, 1.0);\n"
"    vec4 color = mix(innerCol,outerCol,d);\n"
"    // Combine alpha\n"
"    color *= strokeAlpha * scissor;\n"
"    result = color;\n"
"  } else if (type == 1) {    // Image\n"
"    // Calculate color from texture\n"
"    vec2 pt = (paintMat * vec3(fpos,1.0)).xy / extent;\n"
"    vec4 color = texture2D(tex, pt);\n"
"    if (texType == 1) color = vec4(color.xyz*color.w,color.w);\n"
"    if (texType == 2) color = vec4(color.x);\n"
"    // Apply color tint and alpha.\n"
"    color *= innerCol;\n"
"    // Combine alpha\n"
"    color *= strokeAlpha * scissor;\n"
"    result = color;\n"
"  } else if (type == 2) {    // Stencil fill\n"
"    result = vec4(1,1,1,1);\n"
"  } else if (type == 3) {    // Texture atlas\n"
"    vec4 color = texture2D(tex, ftcoord, -0.66);\n"
"    if (texType == 1) color = vec4(color.xyz*color.w,color.w);\n"
"    if (texType == 2) color = vec4(color.x);\n"
"    color *= scissor;\n"
"    result = color * innerCol;\n"
"  }\n"
"  gl_FragColor = result;\n"
"}\n";

static const char *shader_info_log(GLuint shader)
{
  char *buffer = calloc(2048, sizeof(char));
  GLsizei length;
  glGetShaderInfoLog(shader, 2048, &length, buffer);
  if (length >= 2048) length = 2047;
  buffer[length] = '\0';
  return buffer;
}

static const char *program_info_log(GLuint program)
{
  char *buffer = calloc(2048, sizeof(char));
  GLsizei length;
  glGetProgramInfoLog(program, 2048, &length, buffer);
  if (length >= 2048) length = 2047;
  buffer[length] = '\0';
  return buffer;
}

static GLuint create_shader(const char *version, const char *prefix, const char *source, GLenum kind)
{
  const char *buffer[3];
  buffer[0] = version ? version : "";
  buffer[1] = prefix ? prefix : "";
  buffer[2] = source ? source : "";

  GLuint shader = glCreateShader(kind);
  glShaderSource(shader, 3, buffer, 0);
  glCompileShader(shader);

  GLint result = GL_FALSE;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &result);

  if (result != GL_TRUE) {
    const char *log = shader_info_log(shader);
    fprintf(stderr, "ERROR: GL shader %d did not compile\n%s\n", shader, log);
    free((void*)log);
  }

  return shader;
}

static int validate_program(GLuint program)
{
  glValidateProgram(program);
  GLint result = GL_FALSE;
  glGetProgramiv(program, GL_VALIDATE_STATUS, &result);

  if (result != GL_TRUE)
  {
    const char *log = program_info_log(program);
    fprintf(stderr, "ERROR: GL program %d did not compile\n%s\n", program, log);
    free((void*)log);

    return 0;
  }

  return 1;
}

static int create_program(GLuint *program, const char *version, const char *prefix)
{
  GLuint vs, fs, ps;
  vs = create_shader(version, prefix, source_vertex_shader, GL_VERTEX_SHADER),
  fs = create_shader(version, prefix, source_fragment_shader, GL_FRAGMENT_SHADER),
  ps = glCreateProgram();

  glAttachShader(ps, vs);
  glAttachShader(ps, fs);
  glBindAttribLocation(ps, 0, "vertex");
  glBindAttribLocation(ps, 1, "tcoord");

  glLinkProgram(ps);
  GLint result = GL_FALSE;
  glGetProgramiv(ps, GL_LINK_STATUS, &result);

  if (result != GL_TRUE)
  {
    const char *log = program_info_log(ps);
    fprintf(stderr, "ERROR: could not link GL program %d\n%s\n", ps, log);
    free((void*)log);
    return 0;
  }
  if (!validate_program(ps)) abort();
  glDeleteShader(vs);
  glDeleteShader(fs);
  *program = ps;
  return 1;
}

static int gl_state_create(int antialias, gl_state *state)
{
  char buffer[2048];
  GLuint program;

  if (!create_program(&program, NULL, antialias ? "#define EDGE_AA 1\n" : NULL))
    return 0;

  state->program   = program;
  state->viewsize  = glGetUniformLocation(program, "viewSize");
  state->viewxform = glGetUniformLocation(program, "viewXform");
  state->tex       = glGetUniformLocation(program, "tex");
  state->frag      = glGetUniformLocation(program, "frag");
  glGenBuffers(1, &state->vert_vbo);

  state->valid = 1;

  return 1;
}

static void gl_state_delete(gl_state *state)
{
  if (state->valid)
  {
    glDeleteProgram(state->program);
    glDeleteBuffers(1, &state->vert_vbo);
    state->valid = 0;
  }
}

#define Gl_state_val(v) ((gl_state*)(Data_custom_val(v)))

static void gl_state_finalize(value v)
{
  gl_state *state = Gl_state_val(v);
  if (state->valid)
  {
    fprintf(stderr, "wall warning: gl_state collected by Gc, explicit deletion is preferable\n");
    gl_state_delete(state);
  }
}

static struct custom_operations gl_state_custom_ops = {
  .identifier  = "wall_gl_state",
  .finalize    = custom_finalize_default,
  .compare     = custom_compare_default,
  .hash        = custom_hash_default,
  .serialize   = custom_serialize_default,
  .deserialize = custom_deserialize_default
};


CAMLprim value wall_gl_create(value antialias)
{
  gl_state result;

  if (!gl_state_create(Long_val(antialias), &result))
    caml_failwith("wall: cannot initialize OpenGL");

  value v = caml_alloc_custom(&gl_state_custom_ops, sizeof(gl_state), 0, 1);
  *Gl_state_val(v) = result;

  return v;
}

CAMLprim value wall_gl_delete(value v)
{
  gl_state_delete(Gl_state_val(v));
  return Val_unit;
}

CAMLprim value wall_gl_is_valid(value v)
{
  return Val_bool(Gl_state_val(v));
}

CAMLprim value wall_gl_bind_xform(value state, value buf)
{
  float *data = Caml_ba_data_val(buf);
  glUniform3fv(Gl_state_val(state)->viewxform, 3, data);
  return Val_unit;
}

CAMLprim value wall_gl_bind_paint(value state, value buf)
{
  float *data = Caml_ba_data_val(buf);
  glUniform4fv(Gl_state_val(state)->frag, 11, data);
  return Val_unit;
}

CAMLprim value wall_gl_bind_texture(value texture)
{
  glBindTexture(GL_TEXTURE_2D, Long_val(texture));
  return Val_unit;
}


CAMLprim value wall_gl_draw_triangle_fan(value first, value count)
{
  glDrawArrays(GL_TRIANGLE_FAN, Long_val(first), Long_val(count));
  return Val_unit;
}

CAMLprim value wall_gl_draw_triangle_strip(value first, value count)
{
  glDrawArrays(GL_TRIANGLE_STRIP, Long_val(first), Long_val(count));
  return Val_unit;
}

CAMLprim value wall_gl_draw_triangles(value first, value count)
{
  glDrawArrays(GL_TRIANGLES, Long_val(first), Long_val(count));
  return Val_unit;
}

CAMLprim value wall_gl_fill_prepare_stencil(value unit)
{
  glEnable(GL_STENCIL_TEST);
  glStencilMask(0xFF);
  glStencilFunc(GL_ALWAYS, 0x00, 0xFF);
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR_WRAP);
  glStencilOpSeparate(GL_BACK,  GL_KEEP, GL_KEEP, GL_DECR_WRAP);
  glDisable(GL_CULL_FACE);
  return Val_unit;
}

CAMLprim value wall_gl_fill_prepare_cover(value unit)
{
  glEnable(GL_CULL_FACE);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  return Val_unit;
}

CAMLprim value wall_gl_prepare_aa(value unit)
{
  glStencilFunc(GL_EQUAL, 0x00, 0xFF);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  return Val_unit;
}

CAMLprim value wall_gl_fill_finish_and_cover(value first, value count)
{
  glStencilFunc(GL_NOTEQUAL, 0x00, 0xff);
  glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);
  (void)wall_gl_draw_triangle_strip(first, count);
  glDisable(GL_STENCIL_TEST);
  return Val_unit;
}

CAMLprim value wall_gl_stencil_stroke_prepare_stencil(value unit)
{
  glEnable(GL_STENCIL_TEST);
  glStencilMask(0xff);
  glStencilFunc(GL_EQUAL, 0x0, 0xff);
  glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
  return Val_unit;
}

CAMLprim value wall_gl_stencil_stroke_prepare_clear(value unit)
{
  glStencilFunc(GL_ALWAYS, 0x00, 0xFF);
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);
  return Val_unit;
}

CAMLprim value wall_gl_stencil_stroke_finish(value unit)
{
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glDisable(GL_STENCIL_TEST);
  return Val_unit;
}

CAMLprim value wall_gl_set_reversed(value b)
{
  glFrontFace(Long_val(b) ? GL_CW : GL_CCW);
  return Val_unit;
}

CAMLprim value wall_gl_frame_prepare(value t, value width, value height, value data)
{
  CAMLparam4(t, width, height, data);

  gl_state *state = Gl_state_val(t);
  if (!state->valid)
    caml_failwith("wall: use of gl context after delete");

  glUseProgram(state->program);
  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);
  glFrontFace(GL_CCW);
  glEnable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_SCISSOR_TEST);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glStencilMask(0XFFFFFFFF);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glStencilFunc(GL_ALWAYS, 0, 0xFFFFFFFF);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, 0);

  /* Upload vertex data */
  glBindBuffer(GL_ARRAY_BUFFER, state->vert_vbo);
  glBufferData(GL_ARRAY_BUFFER,
      caml_ba_byte_size(Caml_ba_array_val(data)),
      Caml_ba_data_val(data),
      GL_STREAM_DRAW);

  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * 4, (void*)0);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * 4, (void*)(2 * 4));

  /* Set view and texture just once per frame. */
  glUniform1i(state->tex, 0);
  glUniform2f(state->viewsize, Double_val(width), Double_val(height));

  CAMLreturn(Val_unit);
}

CAMLprim value wall_gl_frame_finish(value unit)
{
  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glDisable(GL_CULL_FACE);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glUseProgram(0);
  glBindTexture(GL_TEXTURE_2D, 0);

  return Val_unit;
}

/* Texture */

CAMLprim value wall_gl_texture_create(value unit)
{
  GLuint result = 0;
  glGenTextures(1, &result);
  return Val_long(result);
}

CAMLprim value wall_gl_texture_delete(value t)
{
  GLuint tex = Long_val(t);
  glDeleteTextures(1, &tex);
  return Val_unit;
}

static void *pack_image(unsigned char *data, size_t width, size_t height, size_t stride)
{
  static unsigned char *buffer = NULL;
  if (!data)
  {
    if (buffer)
    {
      free(buffer);
      buffer = NULL;
    }
    return NULL;
  }

  if (stride == width)
    return data;

  if (buffer)
    buffer = realloc(buffer, width * height);
  else
    buffer = malloc(width * height);

  if (!buffer)
    abort();

  for (size_t y = 0; y < height; ++y)
    memcpy(buffer + y * width, data + y * stride, width);

  return buffer;
}

static GLenum gl_format_from_channels(value channels)
{
  switch(Long_val(channels))
  {
    case 1:
      return GL_LUMINANCE;
    case 3:
      return GL_RGB;
    case 4:
      return GL_RGBA;
    default:
      abort();
  }
}

static GLenum gl_type(value is_float)
{
  return Bool_val(is_float) ? GL_FLOAT : GL_UNSIGNED_BYTE;
}

static void gl_tex_param(void)
{
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
}

CAMLprim value wall_gl_texture_upload(value t, value level, value is_float,
    value width, value height, value channels,
    value data, value offset, value stride)
{
  int elem_size = Bool_val(is_float) ? 4 : 1;
  void *ptr = pack_image(Caml_ba_data_val(data),
      Long_val(width) * Long_val(channels) * elem_size,
      Long_val(height), Long_val(stride) * elem_size);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Long_val(t));
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D(GL_TEXTURE_2D, Long_val(level),
      Long_val(channels), Long_val(width), Long_val(height), 0,
      gl_format_from_channels(channels), gl_type(is_float), ptr
      );
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  gl_tex_param();
  pack_image(NULL, 0, 0, 0);
  return Val_unit;
}

CAMLprim value wall_gl_texture_upload_bc(value *argv, int argn)
{
  return wall_gl_texture_upload(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8]
      );
}

CAMLprim value wall_gl_texture_update(value t, value level, value is_float,
    value x, value y, value width, value height, value channels,
    value data, value offset, value stride)
{
  int elem_size = Bool_val(is_float) ? 4 : 1;
  void *ptr = pack_image(Caml_ba_data_val(data),
      Long_val(width) * Long_val(channels) * elem_size,
      Long_val(height), Long_val(stride) * elem_size);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Long_val(t));
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexSubImage2D(GL_TEXTURE_2D, Long_val(level),
      Long_val(x), Long_val(y),
      Long_val(width), Long_val(height),
      gl_format_from_channels(channels), gl_type(is_float), ptr
      );
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  gl_tex_param();
  pack_image(NULL, 0, 0, 0);
  return Val_unit;
}

CAMLprim value wall_gl_texture_update_bc(value *argv, int argn)
{
  return wall_gl_texture_update(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8], argv[9], argv[10]
      );
}

CAMLprim value wall_gl_texture_generate_mipmap(value t)
{
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Long_val(t));
  glGenerateMipmap(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
  return Val_unit;
}

/* Only used to measure deltas, so overflow is not a problem */

#include <time.h>
CAMLprim value wall_time_spent(value unit)
{
  struct timespec tp;
  if (clock_gettime(CLOCK_MONOTONIC, &tp) == 0)
    return Val_long(tp.tv_sec * 1000000000 + tp.tv_nsec);
  else
    return Val_long(0);
}

extern uintnat caml_allocated_words;
extern value *caml_young_alloc_end, *caml_young_ptr;

extern double
  caml_stat_minor_words,
  caml_stat_promoted_words,
  caml_stat_major_words;

CAMLprim value wall_memory_spent(value unit)
{
  long base =
    caml_stat_minor_words + caml_stat_major_words - caml_stat_promoted_words;
  long minwords = (caml_young_alloc_end - caml_young_ptr);
  long majwords = caml_allocated_words;

  return Val_long(base + minwords + majwords);
}
