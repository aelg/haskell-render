#version 310 es

layout(location = 0) in vec4 vertexPosition_modelspace;

void main(){
    gl_Position = vertexPosition_modelspace;
}
