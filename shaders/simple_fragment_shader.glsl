#version 330 core

precision mediump float;

in vec3 fragmentColor;
out vec3 color;

void main(){
  color = fragmentColor;
}
