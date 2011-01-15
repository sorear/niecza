# This is separated out since it's inherently platform specific
# and will only work properly on Unix

use Test;

is "/bin/..".IO.realpath, "/", "realpath chases ..";
is "/bin".IO.append("sh"), "/bin/sh", "append adds /";
is "/bin/".IO.append("sh"), "/bin/sh", "but only if needed";
