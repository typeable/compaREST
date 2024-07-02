{ pkgs ? import <nixpkgs> {} }:

# Make yq work inside whatever `stack test` launches.

let
  yq-patch = pkgs.writeText "yq-tty-fix.patch" ''
diff --git a/yq/__init__.py b/yq/__init__.py
index d95ac4b..ae7d59e 100644
--- a/yq/__init__.py
+++ b/yq/__init__.py
@@ -110,7 +110,7 @@ def cli(args=None, input_format="yaml", program_name="yq"):
     in_place = args.in_place
     delattr(args, "in_place")

-    if sys.stdin.isatty() and not args.input_streams:
+    if (sys.stdin is None or sys.stdin.isatty()) and not args.input_streams:
         parser.print_help()
         sys.exit(2)
     elif not args.input_streams:
  '';
in
pkgs.yq.overrideAttrs (final: old: {
    patches = old.patches ++ [ yq-patch ];
})
