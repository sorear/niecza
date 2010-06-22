# Mock module for testing.  Could be turned into an alternate bootstrap, if
# perlesque doesn't work.

module System;

class Void { ... }
class Int32 { ... }

module Reflection {
    module Emit {
        class DynamicMethod { ... }
        class OpCodes { ... }
    }
}
