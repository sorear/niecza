# vim: ft=perl6
# The bulk of the code for this is in lib/JSYNC.cs
module JSYNC;

sub to-jsync($obj) is export { Q:CgOp { (box Str (to_jsync (@ {$obj}))) } }
sub from-jsync($obj) is export { Q:CgOp { (from_jsync (unbox str (@ {$obj}))) } }
sub to-json($obj) is export { Q:CgOp { (box Str (to_json (@ {$obj}))) } }
sub from-json($obj) is export { Q:CgOp { (from_json (unbox str (@ {$obj}))) } }
