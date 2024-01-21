{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    pkgs.libGL
  ];

  propagatedBuildInputs = [
    pkgs.libGL
    pkgs.SDL2
    pkgs.SDL2_image
    pkgs.SDL2_ttf
    pkgs.libffi
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.SDL2_ttf}/lib:${pkgs.libffi}/lib:${pkgs.SDL2_image}/lib:${pkgs.SDL2}/lib:${pkgs.libGL}/lib:$LD_LIBRARY_PATH"
  '';
}
