# Example Compiled Program

Intermediate representation code:

```rs
/// Create a custom, handwritten assembly builtin for our compiled program to use.
let put = ConstExpr::CoreBuiltin(CoreBuiltin {
    name: "put".to_string(),
    args: vec![("x".to_string(), Type::Int)],
    ret: Type::None,
    body: vec![
        CoreOp::Comment("Put an integer from the stack".to_string()),
        CoreOp::Put(SP.deref()),
        CoreOp::Pop(None, 1),
    ],
});


/// This is roughly equivalent to this psuedocode:
/// 
/// ```
/// put(
///     ('a', (false, 2, ['c', 'd', 'e']), {x='b', y=3, z=true}).1.2[0]
/// )
/// ```
let expr = put.app(vec![Expr::Tuple(vec![
    ConstExpr::Char('a').into(),
    Expr::Tuple(vec![
        ConstExpr::Bool(false).into(),
        ConstExpr::Int(2).into(),
        Expr::Array(vec![
            ConstExpr::Char('c').into(),
            ConstExpr::Char('d').into(),
            ConstExpr::Char('e').into(),
        ]),
    ]),
    Expr::structure(btreemap! {
        "x" => ConstExpr::Char('b').into(),
        "y" => ConstExpr::Int(3).into(),
        "z" => ConstExpr::Bool(true).into(),
    }),
])
.field(ConstExpr::Int(1))  // Get the nested tuple (second field of tuple)
.field(ConstExpr::Int(2))  // Get the array in the nested tuple
.idx(ConstExpr::Int(0))]); // Index with zero (get the 'c')
```

Compiled assembly output from intermediate representation:

```rs
0000: Comment("push char 'a'")
0001: Next(SP, None)
0002: Set([SP], 97)
0003: Comment("push bool false")
0004: Next(SP, None)
0005: Set([SP], 0)
0006: Comment("push int 2")
0007: Next(SP, None)
0008: Set([SP], 2)
0009: Comment("push char 'c'")
000a: Next(SP, None)
000b: Set([SP], 99)
000c: Comment("push char 'd'")
000d: Next(SP, None)
000e: Set([SP], 100)
000f: Comment("push char 'e'")
0010: Next(SP, None)
0011: Set([SP], 101)
0012: Comment("push char 'b'")
0013: Next(SP, None)
0014: Set([SP], 98)
0015: Comment("push int 3")
0016: Next(SP, None)
0017: Set([SP], 3)
0018: Comment("push bool true")
0019: Next(SP, None)
001a: Set([SP], 1)
001b: Copy { src: [SP-7], dst: [SP-8], size: 5 }
001c: Pop(None, 4)
001d: Copy { src: [SP-2], dst: [SP-4], size: 3 }
001e: Pop(None, 2)
001f: Comment("push int 0")
0020: Next(SP, None)
0021: Set([SP], 0)
0022: Pop(Some(B), 1)
0023: Set(A, 1)
0024: Mul { src: A, dst: B }
0025: GetAddress { addr: [SP-2], dst: A }
0026: Index { src: A, offset: B, dst: C }
0027: Copy { src: [C], dst: [SP-2], size: 1 }
0028: Pop(None, 2)
0029: Comment("Put an integer from the stack")
002a: Put([SP])
002b: Pop(None, 1)
```

Virtual machine code:

```rs
0000: Move(9)
0001: Where
0002: Move(-9)
0003: Move(3)
0004: Save
0005: Move(-3)
0006: Move(3)
0007: Deref
0008: Move(16)
0009: Where
000a: Move(-16)
000b: Refer
000c: Move(-3)
000d: Save
000e: Restore
000f: Move(2)
0010: Save
0011: Move(-2)
0012: Comment("push char 'a'")
0013: Deref
0014: Move(1)
0015: Where
0016: Move(-1)
0017: Refer
0018: Save
0019: Set(97)
001a: Deref
001b: Save
001c: Refer
001d: Comment("push bool false")
001e: Deref
001f: Move(1)
0020: Where
0021: Move(-1)
0022: Refer
0023: Save
0024: Set(0)
0025: Deref
0026: Save
0027: Refer
0028: Comment("push int 2")
0029: Deref
002a: Move(1)
002b: Where
002c: Move(-1)
002d: Refer
002e: Save
002f: Set(2)
0030: Deref
0031: Save
0032: Refer
0033: Comment("push char 'c'")
0034: Deref
0035: Move(1)
0036: Where
0037: Move(-1)
0038: Refer
0039: Save
003a: Set(99)
003b: Deref
003c: Save
003d: Refer
003e: Comment("push char 'd'")
003f: Deref
0040: Move(1)
0041: Where
0042: Move(-1)
0043: Refer
0044: Save
0045: Set(100)
0046: Deref
0047: Save
0048: Refer
0049: Comment("push char 'e'")
004a: Deref
004b: Move(1)
004c: Where
004d: Move(-1)
004e: Refer
004f: Save
0050: Set(101)
0051: Deref
0052: Save
0053: Refer
0054: Comment("push char 'b'")
0055: Deref
0056: Move(1)
0057: Where
0058: Move(-1)
0059: Refer
005a: Save
005b: Set(98)
005c: Deref
005d: Save
005e: Refer
005f: Comment("push int 3")
0060: Deref
0061: Move(1)
0062: Where
0063: Move(-1)
0064: Refer
0065: Save
0066: Set(3)
0067: Deref
0068: Save
0069: Refer
006a: Comment("push bool true")
006b: Deref
006c: Move(1)
006d: Where
006e: Move(-1)
006f: Refer
0070: Save
0071: Set(1)
0072: Deref
0073: Save
0074: Refer
0075: Deref
0076: Move(-7)
0077: Restore
0078: Move(7)
0079: Refer
007a: Deref
007b: Move(-8)
007c: Save
007d: Move(8)
007e: Refer
007f: Deref
0080: Move(-6)
0081: Restore
0082: Move(6)
0083: Refer
0084: Deref
0085: Move(-7)
0086: Save
0087: Move(7)
0088: Refer
0089: Deref
008a: Move(-5)
008b: Restore
008c: Move(5)
008d: Refer
008e: Deref
008f: Move(-6)
0090: Save
0091: Move(6)
0092: Refer
0093: Deref
0094: Move(-4)
0095: Restore
0096: Move(4)
0097: Refer
0098: Deref
0099: Move(-5)
009a: Save
009b: Move(5)
009c: Refer
009d: Deref
009e: Move(-3)
009f: Restore
00a0: Move(3)
00a1: Refer
00a2: Deref
00a3: Move(-4)
00a4: Save
00a5: Move(4)
00a6: Refer
00a7: Deref
00a8: Move(-4)
00a9: Where
00aa: Move(4)
00ab: Refer
00ac: Save
00ad: Deref
00ae: Move(-2)
00af: Restore
00b0: Move(2)
00b1: Refer
00b2: Deref
00b3: Move(-4)
00b4: Save
00b5: Move(4)
00b6: Refer
00b7: Deref
00b8: Move(-1)
00b9: Restore
00ba: Move(1)
00bb: Refer
00bc: Deref
00bd: Move(-3)
00be: Save
00bf: Move(3)
00c0: Refer
00c1: Deref
00c2: Restore
00c3: Refer
00c4: Deref
00c5: Move(-2)
00c6: Save
00c7: Move(2)
00c8: Refer
00c9: Deref
00ca: Move(-2)
00cb: Where
00cc: Move(2)
00cd: Refer
00ce: Save
00cf: Comment("push int 0")
00d0: Deref
00d1: Move(1)
00d2: Where
00d3: Move(-1)
00d4: Refer
00d5: Save
00d6: Set(0)
00d7: Deref
00d8: Save
00d9: Refer
00da: Deref
00db: Restore
00dc: Refer
00dd: Move(5)
00de: Save
00df: Move(-5)
00e0: Deref
00e1: Move(-1)
00e2: Where
00e3: Move(1)
00e4: Refer
00e5: Save
00e6: Set(1)
00e7: Move(4)
00e8: Save
00e9: Move(-4)
00ea: Move(5)
00eb: Restore
00ec: Move(-5)
00ed: Move(4)
00ee: Mul
00ef: Move(-4)
00f0: Move(5)
00f1: Save
00f2: Move(-5)
00f3: Deref
00f4: Move(-2)
00f5: Where
00f6: Move(2)
00f7: Refer
00f8: Move(4)
00f9: Save
00fa: Move(-4)
00fb: Move(5)
00fc: Restore
00fd: Move(-5)
00fe: Move(1)
00ff: Save
0100: Move(-1)
0101: Move(4)
0102: Restore
0103: Move(-4)
0104: Move(6)
0105: Save
0106: Move(-6)
0107: Move(1)
0108: Restore
0109: Move(-1)
010a: While
010b:    Move(6)
010c:    Deref
010d:    Move(1)
010e:    Where
010f:    Move(-1)
0110:    Refer
0111:    Move(-6)
0112:    Move(6)
0113:    Save
0114:    Move(-6)
0115:    Move(1)
0116:    Restore
0117:    Dec
0118:    Save
0119:    Move(-1)
011a:    Move(1)
011b:    Restore
011c:    Move(-1)
011d: End
011e: Move(6)
011f: Deref
0120: Restore
0121: Refer
0122: Move(-6)
0123: Deref
0124: Move(-2)
0125: Save
0126: Move(2)
0127: Refer
0128: Deref
0129: Move(-2)
012a: Where
012b: Move(2)
012c: Refer
012d: Save
012e: Comment("Put an integer from the stack")
012f: Deref
0130: Restore
0131: Refer
0132: Put
0133: Deref
0134: Move(-1)
0135: Where
0136: Move(1)
0137: Refer
0138: Save
```