#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/version.h>
#include <time.h>
#include <string.h>
#include <stdio.h>

#include <SDL2/SDL.h>
#define GL3

#ifdef __APPLE__
# include <OpenGL/gl3.h>
# ifndef CLOCK_MONOTONIC
#  define WORKAROUND_APPLE_CLOCK
# endif
# ifdef WORKAROUND_APPLE_CLOCK
#  include <mach/mach_time.h>
# endif
#else
# include <GLES2/gl2.h>
# ifdef GL3
# include <GLES3/gl3.h>
# endif
#endif

typedef struct {
  void (*glActiveTexture) (GLenum texture);
  void (*glAttachShader) (GLuint program, GLuint shader);
  void (*glBindAttribLocation) (GLuint program, GLuint index, const GLchar *name);
  void (*glBindBuffer) (GLenum target, GLuint buffer);
  void (*glBindTexture) (GLenum target, GLuint texture);
  void (*glBindVertexArray) (GLuint array);
  void (*glBlendFunc) (GLenum sfactor, GLenum dfactor);
  void (*glBufferData) (GLenum target, GLsizeiptr size, const void *data, GLenum usage);
  void (*glColorMask) (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
  void (*glCompileShader) (GLuint shader);
  GLuint (*glCreateProgram) (void);
  GLuint (*glCreateShader) (GLenum type);
  void (*glCullFace) (GLenum mode);
  void (*glDeleteBuffers) (GLsizei n, const GLuint *buffers);
  void (*glDeleteProgram) (GLuint program);
  void (*glDeleteShader) (GLuint shader);
  void (*glDeleteTextures) (GLsizei n, const GLuint *textures);
  void (*glDeleteVertexArrays) (GLsizei n, const GLuint *arrays);
  void (*glDisable) (GLenum cap);
  void (*glDisableVertexAttribArray) (GLuint index);
  void (*glDrawArrays) (GLenum mode, GLint first, GLsizei count);
  void (*glEnable) (GLenum cap);
  void (*glEnableVertexAttribArray) (GLuint index);
  void (*glFrontFace) (GLenum mode);
  void (*glGenBuffers) (GLsizei n, GLuint *buffers);
  void (*glGenerateMipmap) (GLenum target);
  void (*glGenTextures) (GLsizei n, GLuint *textures);
  void (*glGenVertexArrays) (GLsizei n, GLuint *arrays);
  void (*glGetProgramInfoLog) (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
  void (*glGetProgramiv) (GLuint program, GLenum pname, GLint *params);
  void (*glGetShaderInfoLog) (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
  void (*glGetShaderiv) (GLuint shader, GLenum pname, GLint *params);
  GLint (*glGetUniformLocation) (GLuint program, const GLchar *name);
  void (*glLinkProgram) (GLuint program);
  void (*glPixelStorei) (GLenum pname, GLint param);
  void (*glShaderSource) (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length);
  void (*glStencilFunc) (GLenum func, GLint ref, GLuint mask);
  void (*glStencilMask) (GLuint mask);
  void (*glStencilOp) (GLenum fail, GLenum zfail, GLenum zpass);
  void (*glStencilOpSeparate) (GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass);
  void (*glTexSubImage2D) (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels);
  void (*glTexParameteri) (GLenum target, GLenum pname, GLint param);
  void (*glUseProgram) (GLuint program);
  void (*glValidateProgram) (GLuint program);
  void (*glVertexAttribPointer) (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer);
  void (*glUniform1i) (GLint location, GLint v0);
  void (*glUniform2f) (GLint location, GLfloat v0, GLfloat v1);
  void (*glTexImage2D) (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels);
  void (*glUniform3fv) (GLint location, GLsizei count, const GLfloat *value);
  void (*glUniform1f) (GLint location, GLfloat v0);
  void (*glUniform4fv) (GLint location, GLsizei count, const GLfloat *value);

  GLuint program, viewsize, viewxform, strokewidth, tex, frag, vert_vbo;
#ifdef GL3
  GLuint vert_vao;
#endif
  int valid;
} gl_state;

static const char *source_vertex_shader =
"uniform vec2 viewSize;\n"
"uniform float strokeWidth;\n"
"uniform vec3 viewXform[3];\n"
"attribute vec2 vertex;\n"
"attribute vec2 tcoord;\n"
"varying vec3 ftcoord;\n"
"varying vec2 fpos;\n"
"\n"
"void main(void) {\n"
"  fpos = (mat3(viewXform[0], viewXform[1], viewXform[2]) * vec3(vertex,1.0)).xy;\n"
"  if (tcoord.x < -1.0)\n"
"  {\n"
"    ftcoord = vec3(- floor(tcoord) / 2.0 - 1.0, 1.0);\n"
"    vec2 d = (fract(tcoord) - 0.5) * 1024.0;\n"
"    float len = length(d);\n"
"    if (len > 0.0001)\n"
"    {\n"
"      vec2 dm = mat2(viewXform[0].xy, viewXform[1].xy) * d;\n"
"      if (strokeWidth > 0.0)\n"
"      {\n"
"        float lenm = length(dm);\n"
"        ftcoord.z = ((strokeWidth * lenm / len + 1.0) * 0.5) * max(ftcoord.y,1.0);\n"
"        ftcoord.y = min(ftcoord.y,1.0);\n"
"      }\n"
"      fpos += normalize(dm) * len;\n"
"    }\n"
"  }\n"
"  else\n"
"  {\n"
"    ftcoord = vec3(tcoord, 1.0);\n"
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
"varying vec3 ftcoord;\n"
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
"  return clamp((0.5-abs(ftcoord.x-0.5))*2.0*ftcoord.z, 0.0, 1.0)*ftcoord.y;\n"
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
"    vec4 color = texture2D(tex, ftcoord.xy, -0.66);\n"
"    if (texType == 1) color = vec4(color.xyz*color.w,color.w);\n"
"    if (texType == 2) color = vec4(color.x);\n"
"    color *= scissor;\n"
"    result = color * innerCol;\n"
"  }\n"
"  gl_FragColor = result;\n"
"}\n";

static const char *shader_info_log(gl_state *state, GLuint shader)
{
  char *buffer = calloc(2048, sizeof(char));
  GLsizei length;
  state->glGetShaderInfoLog(shader, 2048, &length, buffer);
  if (length >= 2048) length = 2047;
  buffer[length] = '\0';
  return buffer;
}

static const char *program_info_log(gl_state *state, GLuint program)
{
  char *buffer = calloc(2048, sizeof(char));
  GLsizei length;
  state->glGetProgramInfoLog(program, 2048, &length, buffer);
  if (length >= 2048) length = 2047;
  buffer[length] = '\0';
  return buffer;
}

static GLuint create_shader(gl_state *state, const char *version, const char *prefix, const char *source, GLenum kind)
{
  const char *buffer[3];
  buffer[0] = version ? version : "";
  buffer[1] = prefix ? prefix : "";
  buffer[2] = source ? source : "";

  GLuint shader = state->glCreateShader(kind);
  state->glShaderSource(shader, 3, buffer, 0);
  state->glCompileShader(shader);

  GLint result = GL_FALSE;
  state->glGetShaderiv(shader, GL_COMPILE_STATUS, &result);

  if (result != GL_TRUE) {
    const char *log = shader_info_log(state, shader);
    fprintf(stderr, "ERROR: GL shader %d did not compile\n%s\n", shader, log);
    free((void*)log);
  }

  return shader;
}

static int validate_program(gl_state *state, GLuint program)
{
  state->glValidateProgram(program);
  GLint result = GL_FALSE;
  state->glGetProgramiv(program, GL_VALIDATE_STATUS, &result);

  if (result != GL_TRUE)
  {
    const char *log = program_info_log(state, program);
    fprintf(stderr, "ERROR: GL program %d did not compile\n%s\n", program, log);
    free((void*)log);

    return 0;
  }

  return 1;
}

static int create_program(gl_state *state, GLuint *program, const char *version, const char *prefix)
{
  GLuint vs, fs, ps;
  vs = create_shader(state, version, prefix, source_vertex_shader, GL_VERTEX_SHADER),
  fs = create_shader(state, version, prefix, source_fragment_shader, GL_FRAGMENT_SHADER),
  ps = state->glCreateProgram();

  state->glAttachShader(ps, vs);
  state->glAttachShader(ps, fs);
  state->glBindAttribLocation(ps, 0, "vertex");
  state->glBindAttribLocation(ps, 1, "tcoord");

  state->glLinkProgram(ps);
  GLint result = GL_FALSE;
  state->glGetProgramiv(ps, GL_LINK_STATUS, &result);

  if (result != GL_TRUE)
  {
    const char *log = program_info_log(state, ps);
    fprintf(stderr, "ERROR: could not link GL program %d\n%s\n", ps, log);
    free((void*)log);
    return 0;
  }
  if (!validate_program(state, ps)) abort();
  state->glDeleteShader(vs);
  state->glDeleteShader(fs);
  *program = ps;
  return 1;
}

static void gl_state_initialize(gl_state *state)
{
  *(void**)&state->glActiveTexture = SDL_GL_GetProcAddress("glActiveTexture");
  *(void**)&state->glAttachShader = SDL_GL_GetProcAddress("glAttachShader");
  *(void**)&state->glBindAttribLocation = SDL_GL_GetProcAddress("glBindAttribLocation");
  *(void**)&state->glBindBuffer = SDL_GL_GetProcAddress("glBindBuffer");
  *(void**)&state->glBindTexture = SDL_GL_GetProcAddress("glBindTexture");
  *(void**)&state->glBindVertexArray = SDL_GL_GetProcAddress("glBindVertexArray");
  *(void**)&state->glBlendFunc = SDL_GL_GetProcAddress("glBlendFunc");
  *(void**)&state->glBufferData = SDL_GL_GetProcAddress("glBufferData");
  *(void**)&state->glColorMask = SDL_GL_GetProcAddress("glColorMask");
  *(void**)&state->glCompileShader = SDL_GL_GetProcAddress("glCompileShader");
  *(void**)&state->glCreateProgram = SDL_GL_GetProcAddress("glCreateProgram");
  *(void**)&state->glCreateShader = SDL_GL_GetProcAddress("glCreateShader");
  *(void**)&state->glCullFace = SDL_GL_GetProcAddress("glCullFace");
  *(void**)&state->glDeleteBuffers = SDL_GL_GetProcAddress("glDeleteBuffers");
  *(void**)&state->glDeleteProgram = SDL_GL_GetProcAddress("glDeleteProgram");
  *(void**)&state->glDeleteShader = SDL_GL_GetProcAddress("glDeleteShader");
  *(void**)&state->glDeleteTextures = SDL_GL_GetProcAddress("glDeleteTextures");
  *(void**)&state->glDeleteVertexArrays = SDL_GL_GetProcAddress("glDeleteVertexArrays");
  *(void**)&state->glDisable = SDL_GL_GetProcAddress("glDisable");
  *(void**)&state->glDisableVertexAttribArray = SDL_GL_GetProcAddress("glDisableVertexAttribArray");
  *(void**)&state->glDrawArrays = SDL_GL_GetProcAddress("glDrawArrays");
  *(void**)&state->glEnable = SDL_GL_GetProcAddress("glEnable");
  *(void**)&state->glEnableVertexAttribArray = SDL_GL_GetProcAddress("glEnableVertexAttribArray");
  *(void**)&state->glFrontFace = SDL_GL_GetProcAddress("glFrontFace");
  *(void**)&state->glGenBuffers = SDL_GL_GetProcAddress("glGenBuffers");
  *(void**)&state->glGenerateMipmap = SDL_GL_GetProcAddress("glGenerateMipmap");
  *(void**)&state->glGenTextures = SDL_GL_GetProcAddress("glGenTextures");
  *(void**)&state->glGenVertexArrays = SDL_GL_GetProcAddress("glGenVertexArrays");
  *(void**)&state->glGetProgramInfoLog = SDL_GL_GetProcAddress("glGetProgramInfoLog");
  *(void**)&state->glGetProgramiv = SDL_GL_GetProcAddress("glGetProgramiv");
  *(void**)&state->glGetShaderInfoLog = SDL_GL_GetProcAddress("glGetShaderInfoLog");
  *(void**)&state->glGetShaderiv = SDL_GL_GetProcAddress("glGetShaderiv");
  *(void**)&state->glGetUniformLocation = SDL_GL_GetProcAddress("glGetUniformLocation");
  *(void**)&state->glLinkProgram = SDL_GL_GetProcAddress("glLinkProgram");
  *(void**)&state->glPixelStorei = SDL_GL_GetProcAddress("glPixelStorei");
  *(void**)&state->glShaderSource = SDL_GL_GetProcAddress("glShaderSource");
  *(void**)&state->glStencilFunc = SDL_GL_GetProcAddress("glStencilFunc");
  *(void**)&state->glStencilMask = SDL_GL_GetProcAddress("glStencilMask");
  *(void**)&state->glStencilOp = SDL_GL_GetProcAddress("glStencilOp");
  *(void**)&state->glStencilOpSeparate = SDL_GL_GetProcAddress("glStencilOpSeparate");
  *(void**)&state->glTexSubImage2D = SDL_GL_GetProcAddress("glTexSubImage2D");
  *(void**)&state->glTexParameteri = SDL_GL_GetProcAddress("glTexParameteri");
  *(void**)&state->glUseProgram = SDL_GL_GetProcAddress("glUseProgram");
  *(void**)&state->glValidateProgram = SDL_GL_GetProcAddress("glValidateProgram");
  *(void**)&state->glVertexAttribPointer = SDL_GL_GetProcAddress("glVertexAttribPointer");
  *(void**)&state->glUniform1i = SDL_GL_GetProcAddress("glUniform1i");
  *(void**)&state->glUniform2f = SDL_GL_GetProcAddress("glUniform2f");
  *(void**)&state->glTexImage2D = SDL_GL_GetProcAddress("glTexImage2D");
  *(void**)&state->glUniform3fv = SDL_GL_GetProcAddress("glUniform3fv");
  *(void**)&state->glUniform1f = SDL_GL_GetProcAddress("glUniform1f");
  *(void**)&state->glUniform4fv = SDL_GL_GetProcAddress("glUniform4fv");
}

static int gl_state_create(gl_state *state, int antialias)
{
  GLuint program;

  gl_state_initialize(state);
  if (!create_program(state, &program, NULL, antialias ? "#define EDGE_AA 1\n" : NULL))
    return 0;

  state->program   = program;
  state->viewsize  = state->glGetUniformLocation(program, "viewSize");
  state->viewxform = state->glGetUniformLocation(program, "viewXform");
  state->strokewidth = state->glGetUniformLocation(program, "strokeWidth");
  state->tex       = state->glGetUniformLocation(program, "tex");
  state->frag      = state->glGetUniformLocation(program, "frag");
#ifdef GL3
  state->glGenVertexArrays(1, &state->vert_vao);
#endif
  state->glGenBuffers(1, &state->vert_vbo);

  state->valid = 1;

  return 1;
}

static void gl_state_delete(gl_state *state)
{
  if (state->valid)
  {
    state->glDeleteProgram(state->program);
#ifdef GL3
    state->glDeleteVertexArrays(1, &state->vert_vao);
#endif
    state->glDeleteBuffers(1, &state->vert_vbo);
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
  .finalize    = gl_state_finalize,
  .compare     = custom_compare_default,
  .hash        = custom_hash_default,
  .serialize   = custom_serialize_default,
  .deserialize = custom_deserialize_default
};


CAMLprim value wall_gl_create(value antialias)
{
  gl_state result;

  if (!gl_state_create(&result, Long_val(antialias)))
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

CAMLprim value wall_gl_bind_xform(value t, value buf)
{
  gl_state *state = Gl_state_val(t);
  float *data = Caml_ba_data_val(buf);
  state->glUniform3fv(state->viewxform, 3, data);
  return Val_unit;
}

CAMLprim value wall_gl_bind_paint(value t, value buf)
{
  gl_state *state = Gl_state_val(t);
  float *data = Caml_ba_data_val(buf);
  state->glUniform1f(state->strokewidth, data[40]);
  state->glUniform4fv(state->frag, 11, data);
  return Val_unit;
}

CAMLprim value wall_gl_bind_texture(value t, value texture)
{
  gl_state *state = Gl_state_val(t);
  state->glBindTexture(GL_TEXTURE_2D, Long_val(texture));
  return Val_unit;
}


CAMLprim value wall_gl_draw_triangle_fan(value t, value first, value count)
{
  gl_state *state = Gl_state_val(t);
  state->glDrawArrays(GL_TRIANGLE_FAN, Long_val(first), Long_val(count));
  return Val_unit;
}

CAMLprim value wall_gl_draw_triangle_strip(value t, value first, value count)
{
  gl_state *state = Gl_state_val(t);
  state->glDrawArrays(GL_TRIANGLE_STRIP, Long_val(first), Long_val(count));
  return Val_unit;
}

CAMLprim value wall_gl_draw_triangles(value t, value first, value count)
{
  gl_state *state = Gl_state_val(t);
  state->glDrawArrays(GL_TRIANGLES, Long_val(first), Long_val(count));
  return Val_unit;
}

CAMLprim value wall_gl_fill_prepare_stencil(value t)
{
  gl_state *state = Gl_state_val(t);
  state->glEnable(GL_STENCIL_TEST);
  state->glStencilMask(0xFF);
  state->glStencilFunc(GL_ALWAYS, 0x00, 0xFF);
  state->glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  state->glStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR_WRAP);
  state->glStencilOpSeparate(GL_BACK,  GL_KEEP, GL_KEEP, GL_DECR_WRAP);
  state->glDisable(GL_CULL_FACE);
  return Val_unit;
}

CAMLprim value wall_gl_fill_prepare_cover(value t)
{
  gl_state *state = Gl_state_val(t);
  state->glEnable(GL_CULL_FACE);
  state->glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  return Val_unit;
}

CAMLprim value wall_gl_prepare_aa(value t)
{
  gl_state *state = Gl_state_val(t);
  state->glStencilFunc(GL_EQUAL, 0x00, 0xFF);
  state->glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  return Val_unit;
}

CAMLprim value wall_gl_fill_finish_and_cover(value t, value first, value count)
{
  gl_state *state = Gl_state_val(t);
  state->glStencilFunc(GL_NOTEQUAL, 0x00, 0xff);
  state->glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);
  (void)wall_gl_draw_triangle_strip(t, first, count);
  state->glDisable(GL_STENCIL_TEST);
  return Val_unit;
}

CAMLprim value wall_gl_stencil_stroke_prepare_stencil(value t)
{
  gl_state *state = Gl_state_val(t);
  state->glEnable(GL_STENCIL_TEST);
  state->glStencilMask(0xff);
  state->glStencilFunc(GL_EQUAL, 0x0, 0xff);
  state->glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
  return Val_unit;
}

CAMLprim value wall_gl_stencil_stroke_prepare_clear(value t)
{
  gl_state *state = Gl_state_val(t);
  state->glStencilFunc(GL_ALWAYS, 0x00, 0xFF);
  state->glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  state->glStencilOp(GL_ZERO, GL_ZERO, GL_ZERO);
  return Val_unit;
}

CAMLprim value wall_gl_stencil_stroke_finish(value t)
{
  gl_state *state = Gl_state_val(t);
  state->glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  state->glDisable(GL_STENCIL_TEST);
  return Val_unit;
}

CAMLprim value wall_gl_set_reversed(value t, value b)
{
  gl_state *state = Gl_state_val(t);
  state->glFrontFace(Long_val(b) ? GL_CW : GL_CCW);
  return Val_unit;
}

CAMLprim value wall_gl_frame_prepare(value t, value width, value height, value data, value datasize)
{
  CAMLparam4(t, width, height, data);

  gl_state *state = Gl_state_val(t);
  if (!state->valid)
    caml_failwith("wall: use of gl context after delete");

  state->glUseProgram(state->program);
  state->glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  state->glEnable(GL_CULL_FACE);
  state->glCullFace(GL_BACK);
  state->glFrontFace(GL_CCW);
  state->glEnable(GL_BLEND);
  state->glDisable(GL_DEPTH_TEST);
  state->glDisable(GL_SCISSOR_TEST);
  state->glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  state->glStencilMask(0XFFFFFFFF);
  state->glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  state->glStencilFunc(GL_ALWAYS, 0, 0xFFFFFFFF);
  state->glActiveTexture(GL_TEXTURE0);
  state->glBindTexture(GL_TEXTURE_2D, 0);

  /* Upload vertex data */
#ifdef GL3
  state->glBindVertexArray(state->vert_vao);
#endif
  state->glBindBuffer(GL_ARRAY_BUFFER, state->vert_vbo);

  long size = Long_val(datasize);
  if (size < 0 || size > caml_ba_byte_size(Caml_ba_array_val(data)))
    abort();
  state->glBufferData(GL_ARRAY_BUFFER, size, Caml_ba_data_val(data), GL_STREAM_DRAW);

  state->glEnableVertexAttribArray(0);
  state->glEnableVertexAttribArray(1);
  state->glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * 4, (void*)0);
  state->glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * 4, (void*)(2 * 4));

  /* Set view and texture just once per frame. */
  state->glUniform1i(state->tex, 0);
  state->glUniform2f(state->viewsize, Double_val(width), Double_val(height));

  CAMLreturn(Val_unit);
}

CAMLprim value wall_gl_frame_finish(value t)
{
  gl_state *state = Gl_state_val(t);
  state->glDisableVertexAttribArray(0);
  state->glDisableVertexAttribArray(1);
  state->glDisable(GL_CULL_FACE);
  state->glBindBuffer(GL_ARRAY_BUFFER, 0);
#ifdef GL3
  state->glBindVertexArray(0);
#endif
  state->glUseProgram(0);
  state->glBindTexture(GL_TEXTURE_2D, 0);

  return Val_unit;
}

/* Texture */

CAMLprim value wall_gl_texture_create(value t)
{
  gl_state *state = Gl_state_val(t);
  GLuint result = 0;
  state->glGenTextures(1, &result);
  return Val_long(result);
}

CAMLprim value wall_gl_texture_delete(value t)
{
  gl_state *state = Gl_state_val(t);
  GLuint tex = Long_val(t);
  state->glDeleteTextures(1, &tex);
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

  size_t y;
  for (y = 0; y < height; ++y)
    memcpy(buffer + y * width, data + y * stride, width);

  return buffer;
}

static GLenum gl_format_from_channels(value channels)
{
  switch(Long_val(channels))
  {
    case 1:
#ifdef __APPLE__
      return GL_RED;
#else
      return GL_LUMINANCE;
#endif
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

static void gl_tex_param(gl_state *state)
{
  state->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  state->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  state->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  state->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
}

CAMLprim value wall_gl_texture_upload(value st,
    value t, value level, value is_float,
    value width, value height, value channels,
    value data, value offset, value stride)
{
  gl_state *state = Gl_state_val(st);
  int elem_size = Bool_val(is_float) ? 4 : 1;
  void *ptr = pack_image(Caml_ba_data_val(data),
      Long_val(width) * Long_val(channels) * elem_size,
      Long_val(height), Long_val(stride) * elem_size);
  state->glActiveTexture(GL_TEXTURE0);
  state->glBindTexture(GL_TEXTURE_2D, Long_val(t));
  state->glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  state->glTexImage2D(GL_TEXTURE_2D, Long_val(level),
      Long_val(channels), Long_val(width), Long_val(height), 0,
      gl_format_from_channels(channels), gl_type(is_float), ptr
      );
  state->glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  gl_tex_param(state);
  pack_image(NULL, 0, 0, 0);
  return Val_unit;
}

CAMLprim value wall_gl_texture_upload_bc(value *argv, int argn)
{
  return wall_gl_texture_upload(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8], argv[9]
      );
}

CAMLprim value wall_gl_texture_update(value st,
    value t, value level, value is_float,
    value x, value y, value width, value height, value channels,
    value data, value offset, value stride)
{
  gl_state *state = Gl_state_val(st);
  int elem_size = Bool_val(is_float) ? 4 : 1;
  void *ptr = pack_image(Caml_ba_data_val(data),
      Long_val(width) * Long_val(channels) * elem_size,
      Long_val(height), Long_val(stride) * elem_size);
  state->glActiveTexture(GL_TEXTURE0);
  state->glBindTexture(GL_TEXTURE_2D, Long_val(t));
  state->glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  state->glTexSubImage2D(GL_TEXTURE_2D, Long_val(level),
      Long_val(x), Long_val(y),
      Long_val(width), Long_val(height),
      gl_format_from_channels(channels), gl_type(is_float), ptr
      );
  state->glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  gl_tex_param(state);
  pack_image(NULL, 0, 0, 0);
  return Val_unit;
}

CAMLprim value wall_gl_texture_update_bc(value *argv, int argn)
{
  return wall_gl_texture_update(
      argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
      argv[6], argv[7], argv[8], argv[9], argv[10], argv[11]
      );
}

CAMLprim value wall_gl_texture_generate_mipmap(value st, value t)
{
  gl_state *state = Gl_state_val(st);
  state->glActiveTexture(GL_TEXTURE0);
  state->glBindTexture(GL_TEXTURE_2D, Long_val(t));
  state->glGenerateMipmap(GL_TEXTURE_2D);
  state->glBindTexture(GL_TEXTURE_2D, 0);
  return Val_unit;
}

CAMLprim value wall_blit_sub_array(value vsrc, value vdst, value vsrco, value vdsto, value vlen)
{
  void *src = Caml_ba_data_val(vsrc);
  void *dst = Caml_ba_data_val(vdst);
  long srcs = caml_ba_byte_size(Caml_ba_array_val(vsrc));
  long dsts = caml_ba_byte_size(Caml_ba_array_val(vdst));
  long srco = Long_val(vsrco);
  long dsto = Long_val(vdsto);
  long len  = Long_val(vlen);

  if ((len < 0 || srco < 0 || dsto < 0) ||
      (srco + len > srcs || dsto + len > dsts))
    abort();
  memmove(dst + dsto, src + srco, len);
  return Val_unit;
}

/* Only used to measure deltas, so overflow is not a problem */

#ifdef WORKAROUND_APPLE_CLOCK
static double microseconds_per_clockticks(void)
{
  static double f = 0.0;
  if (f == 0.0)
  {
    mach_timebase_info_data_t timebase;
    mach_timebase_info(&timebase);
    f = (double)timebase.numer / timebase.denom / 1000.0;
  }
  return f;
}
#endif

CAMLprim value wall_time_spent(value unit)
{
#ifdef WORKAROUND_APPLE_CLOCK
  return Val_long(mach_absolute_time() * microseconds_per_clockticks());
#else
  struct timespec tp;
  if (clock_gettime(CLOCK_MONOTONIC, &tp) == 0)
    return Val_long(tp.tv_sec * 1000000 + tp.tv_nsec / 1000);
  return Val_long(0);
#endif
}

#if OCAML_VERSION < 41000

extern uintnat caml_allocated_words;
extern value *caml_young_alloc_end, *caml_young_ptr;

extern double
  caml_stat_minor_words,
  caml_stat_promoted_words,
  caml_stat_major_words;

#else

#define CAML_INTERNALS

#include <caml/gc_ctrl.h>
#include <caml/minor_gc.h>
#include <caml/major_gc.h>

#endif

CAMLprim value wall_memory_spent(value unit)
{
  long base =
    caml_stat_minor_words + caml_stat_major_words - caml_stat_promoted_words;
  long minwords = (caml_young_alloc_end - caml_young_ptr);
  long majwords = caml_allocated_words;

  return Val_long(base + minwords + majwords);
}
