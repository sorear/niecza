.sub 'x1' # 1
    .return()
.end

.sub 'x2' # 3
    .const 'Sub' $P0 = 'x1'
    $P0()
    $P0()
    .return()
.end

.sub 'x3' # 7
    .const 'Sub' $P0 = 'x2'
    $P0()
    $P0()
    .return()
.end

.sub 'x4' # 15
    .const 'Sub' $P0 = 'x3'
    $P0()
    $P0()
    .return()
.end

.sub 'x5' # 31
    .const 'Sub' $P0 = 'x4'
    $P0()
    $P0()
    .return()
.end

.sub 'x6' # 63
    .const 'Sub' $P0 = 'x5'
    $P0()
    $P0()
    .return()
.end

.sub 'x7' # 127
    .const 'Sub' $P0 = 'x6'
    $P0()
    $P0()
    .return()
.end

.sub 'x8' # 255
    .const 'Sub' $P0 = 'x7'
    $P0()
    $P0()
    .return()
.end

.sub 'x9' # 511
    .const 'Sub' $P0 = 'x8'
    $P0()
    $P0()
    .return()
.end

.sub 'x10' # 1023
    .const 'Sub' $P0 = 'x9'
    $P0()
    $P0()
    .return()
.end

.sub 'x11' # 2047
    .const 'Sub' $P0 = 'x10'
    $P0()
    $P0()
    .return()
.end

.sub 'x12' # 4095
    .const 'Sub' $P0 = 'x11'
    $P0()
    $P0()
    .return()
.end

.sub 'x13' # 8191
    .const 'Sub' $P0 = 'x12'
    $P0()
    $P0()
    .return()
.end

.sub 'x14' # 16383
    .const 'Sub' $P0 = 'x13'
    $P0()
    $P0()
    .return()
.end

.sub 'x15' # 32767
    .const 'Sub' $P0 = 'x14'
    $P0()
    $P0()
    .return()
.end

.sub 'x16' # 65535
    .const 'Sub' $P0 = 'x15'
    $P0()
    $P0()
    .return()
.end

.sub 'x17' # 131071
    .const 'Sub' $P0 = 'x16'
    $P0()
    $P0()
    .return()
.end

.sub 'x18' # 262143
    .const 'Sub' $P0 = 'x17'
    $P0()
    $P0()
    .return()
.end

.sub 'x19' # 524287
    .const 'Sub' $P0 = 'x18'
    $P0()
    $P0()
    .return()
.end

.sub 'x20' # 1048575
    .const 'Sub' $P0 = 'x19'
    $P0()
    $P0()
    .return()
.end

.sub 'x21' # 2097151
    .const 'Sub' $P0 = 'x20'
    $P0()
    $P0()
    .return()
.end

.sub 'x22' # 4194303
    .const 'Sub' $P0 = 'x21'
    $P0()
    $P0()
    .return()
.end

.sub 'x23' :main # 8388607
    .const 'Sub' $P0 = 'x22'
    $P0()
    $P0()
    .return()
.end

.sub 'x24' # 16777215
    .const 'Sub' $P0 = 'x23'
    $P0()
    $P0()
    .return()
.end

.sub 'x25' # 33554431
    .const 'Sub' $P0 = 'x24'
    $P0()
    $P0()
    .return()
.end

.sub 'x26' # 67108863
    .const 'Sub' $P0 = 'x25'
    $P0()
    $P0()
    .return()
.end

.sub 'x27' # 134217727
    .const 'Sub' $P0 = 'x26'
    $P0()
    $P0()
    .return()
.end

.sub 'x28' # 268435455
    .const 'Sub' $P0 = 'x27'
    $P0()
    $P0()
    .return()
.end

.sub 'x29' # 536870911
    .const 'Sub' $P0 = 'x28'
    $P0()
    $P0()
    .return()
.end

.sub 'x30' # 1073741823
    .const 'Sub' $P0 = 'x29'
    $P0()
    $P0()
    .return()
.end

.sub 'x31' # 2147483647
    .const 'Sub' $P0 = 'x30'
    $P0()
    $P0()
    .return()
.end

.sub 'x32' # 4294967295
    .const 'Sub' $P0 = 'x31'
    $P0()
    $P0()
    .return()
.end

.sub 'x33' # 8589934591
    .const 'Sub' $P0 = 'x32'
    $P0()
    $P0()
    .return()
.end

.sub 'x34' # 17179869183
    .const 'Sub' $P0 = 'x33'
    $P0()
    $P0()
    .return()
.end

.sub 'x35' # 34359738367
    .const 'Sub' $P0 = 'x34'
    $P0()
    $P0()
    .return()
.end

.sub 'x36' # 68719476735
    .const 'Sub' $P0 = 'x35'
    $P0()
    $P0()
    .return()
.end

.sub 'x37' # 137438953471
    .const 'Sub' $P0 = 'x36'
    $P0()
    $P0()
    .return()
.end

.sub 'x38' # 274877906943
    .const 'Sub' $P0 = 'x37'
    $P0()
    $P0()
    .return()
.end

.sub 'x39' # 549755813887
    .const 'Sub' $P0 = 'x38'
    $P0()
    $P0()
    .return()
.end

.sub 'x40' # 1099511627775
    .const 'Sub' $P0 = 'x39'
    $P0()
    $P0()
    .return()
.end

