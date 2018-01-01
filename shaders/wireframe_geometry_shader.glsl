#version 330 core

layout(triangles) in;
layout(line_strip, max_vertices = 9) out;

in vec4 normal[];

void main()
{
  int i;
  vec4 offset = vec4(0, 0, -0.001, 0);

  for(i = 0; i < 3; i++)
  {
    gl_Position = gl_in[i].gl_Position + offset;
    EmitVertex();
  }
  EndPrimitive();
  for(i = 0; i < 3; i++)
  {
    gl_Position = gl_in[i].gl_Position + offset;
    EmitVertex();
    gl_Position = normal[i] + offset;
    EmitVertex();
    EndPrimitive();
  }
  EndPrimitive();
}