(in-package :cl-user)
(defpackage glisph.shader
      (:use :cl)
      (:export :+glyph-vs+
               :+glyph-fs+
               :+bounding-box-vs+
               :+bounding-box-fs+))
(in-package :glisph.shader)

;; Glyph vertex
(defvar +glyph-vs+ "#version 130
precision mediump float;
attribute vec2 vertex;
attribute vec2 attrib;
uniform mat4 translationMatrix;
uniform mat4 scaleMatrix;
uniform mat4 rotateMatrix;
varying vec2 p;
void main(void) {
  gl_Position = scaleMatrix * translationMatrix * rotateMatrix * vec4(vertex.x, 1.0-vertex.y, 0.0, 1.0);
  p = attrib;
}
")

(defvar +glyph-fs+ "#version 130
precision mediump float;
varying vec2 p;
void main(void) {
    vec2 px = dFdx(p);
    vec2 py = dFdy(p);
    float fx = (2.0*p.x)*px.x - px.y;
    float fy = (2.0*p.x)*py.x - py.y;
    float sd = (p.x*p.x - p.y)/sqrt(fx*fx + fy*fy);
    float alpha = 0.5 - sd;
    if (alpha > 1.0) {
        // inside
        gl_FragColor = vec4(1.0);
    } else if (alpha < 0.0) {
        // outside
        discard;
    } else {
        // near boundary
        gl_FragColor = vec4(alpha);
    }
}")

;; Bounding box
(defvar +bounding-box-vs+ "#version 130
precision mediump float;
attribute vec2 vertex;
uniform mat4 translationMatrix;
uniform mat4 scaleMatrix;
uniform mat4 rotateMatrix;
void main(void) {
    gl_Position = scaleMatrix * translationMatrix * rotateMatrix * vec4(vertex.x, 1.0-vertex.y, 0.0, 1.0);
}
")

(defvar +bounding-box-fs+ "#version 130
uniform vec4 color;
void main(void) {
    gl_FragColor = color;
}
")

