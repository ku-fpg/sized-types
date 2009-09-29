-- | Sized types X0 to X256.

{-# LANGUAGE TypeFamilies, EmptyDataDecls, UndecidableInstances, ScopedTypeVariables  #-}
module Data.Sized.Ix 
	( X0
	, X1
	, X2
	, X3
	, X4
	, X5
	, X6
	, X7
	, X8
	, X9
	, X10
	, X11
	, X12
	, X13
	, X14
	, X15
	, X16
	, X17
	, X18
	, X19
	, X20
	, X21
	, X22
	, X23
	, X24
	, X25
	, X26
	, X27
	, X28
	, X29
	, X30
	, X31
	, X32
	, X33
	, X34
	, X35
	, X36
	, X37
	, X38
	, X39
	, X40
	, X41
	, X42
	, X43
	, X44
	, X45
	, X46
	, X47
	, X48
	, X49
	, X50
	, X51
	, X52
	, X53
	, X54
	, X55
	, X56
	, X57
	, X58
	, X59
	, X60
	, X61
	, X62
	, X63
	, X64
	, X65
	, X66
	, X67
	, X68
	, X69
	, X70
	, X71
	, X72
	, X73
	, X74
	, X75
	, X76
	, X77
	, X78
	, X79
	, X80
	, X81
	, X82
	, X83
	, X84
	, X85
	, X86
	, X87
	, X88
	, X89
	, X90
	, X91
	, X92
	, X93
	, X94
	, X95
	, X96
	, X97
	, X98
	, X99
	, X100
	, X101
	, X102
	, X103
	, X104
	, X105
	, X106
	, X107
	, X108
	, X109
	, X110
	, X111
	, X112
	, X113
	, X114
	, X115
	, X116
	, X117
	, X118
	, X119
	, X120
	, X121
	, X122
	, X123
	, X124
	, X125
	, X126
	, X127
	, X128
	, X129
	, X130
	, X131
	, X132
	, X133
	, X134
	, X135
	, X136
	, X137
	, X138
	, X139
	, X140
	, X141
	, X142
	, X143
	, X144
	, X145
	, X146
	, X147
	, X148
	, X149
	, X150
	, X151
	, X152
	, X153
	, X154
	, X155
	, X156
	, X157
	, X158
	, X159
	, X160
	, X161
	, X162
	, X163
	, X164
	, X165
	, X166
	, X167
	, X168
	, X169
	, X170
	, X171
	, X172
	, X173
	, X174
	, X175
	, X176
	, X177
	, X178
	, X179
	, X180
	, X181
	, X182
	, X183
	, X184
	, X185
	, X186
	, X187
	, X188
	, X189
	, X190
	, X191
	, X192
	, X193
	, X194
	, X195
	, X196
	, X197
	, X198
	, X199
	, X200
	, X201
	, X202
	, X203
	, X204
	, X205
	, X206
	, X207
	, X208
	, X209
	, X210
	, X211
	, X212
	, X213
	, X214
	, X215
	, X216
	, X217
	, X218
	, X219
	, X220
	, X221
	, X222
	, X223
	, X224
	, X225
	, X226
	, X227
	, X228
	, X229
	, X230
	, X231
	, X232
	, X233
	, X234
	, X235
	, X236
	, X237
	, X238
	, X239
	, X240
	, X241
	, X242
	, X243
	, X244
	, X245
	, X246
	, X247
	, X248
	, X249
	, X250
	, X251
	, X252
	, X253
	, X254
	, X255
	, X256
	, Size(..)
	, Index
	, Row
	, Column
	, coerceSize
	, ADD
	, SUB
	) where
	
import Data.Ix
import Data.Sized.Arith

--- because of TH's lack of type families, will be added later.
type family Index a
type family Row a
type family Column a

class (Eq ix, Ord ix, Show ix, Ix ix, Bounded ix) => Size ix where
	-- | return the size (number of possible elements) in type 'ix'.
	size     :: ix -> Int
	-- | add an arbitary index to a specific 'ix' position.
	addIndex :: ix -> Index ix -> ix
	-- | look at an 'ix' as an 'Index', typically just an 'Int'.
	toIndex  :: ix -> Index ix
	-- | project any 2D array position onto any array. Helper method for 'show'.
	seeIn2D	 :: (Row ix, Column ix) -> ix

	-- TO CONSIDER: ADDing a zero method? This will allow coerseSize to 
	-- work in 2D, and drop the Enum requrement.

type instance Index (a,b) = (Index a,Index b)
type instance Row (a,b)  = a
type instance Column (a,b)  = b

instance (Size x, Size y) => Size (x,y) where
	size (a,b) = size a * size b
	addIndex (a,b) (a',b') = (addIndex a a',addIndex b b')
	toIndex (a,b) = (toIndex a, toIndex b)
	seeIn2D (x,y) = (x,y)
	
type instance Index (a,b,c) = (Index a,Index b,Index c)
-- type instance Row (a,b,c)  = a
--type instance Column (a,b,c)  = (b,c)

instance (Size x, Size y, Size z) => Size (x,y,z) where
	size (a,b,c) = size a * size b * size c
	addIndex (a,b,c) (a',b',c') = (addIndex a a',addIndex b b',addIndex c c')
	toIndex (a,b,c) = (toIndex a, toIndex b,toIndex c)
	seeIn2D (_a,_b) = error "Can not display 3D matrix in 2D"
	
type instance Index (a,b,c,d) = (Index a,Index b,Index c,Index d)

instance (Size x, Size y, Size z,Size z2) => Size (x,y,z,z2) where
	size (a,b,c,d) = size a * size b * size c * size d
	addIndex (a,b,c,d) (a',b',c',d') = (addIndex a a',addIndex b b',addIndex c c',addIndex d d')
	toIndex (a,b,c,d) = (toIndex a, toIndex b,toIndex c,toIndex d)
	seeIn2D (_a,_b) = error "Can not display 4D matrix in 2D"

-- | A good way of converting from one index type to another index type, typically in another base.
coerceSize :: (Index ix1 ~ Index ix2, Size ix1, Size ix2, Num ix2) => ix1 -> ix2
coerceSize ix = addIndex 0 (toIndex ix)

type instance Index X0  = Int
type instance Row X0    = X1
type instance Column X0 = X0

instance Size X0 where
	size _ = 0
	addIndex X0 _n = X0	-- TODO: fix bounds issues
	toIndex X0 = 0
	seeIn2D (_,y) = y

instance Size a => Bounded (X1_ a) where
	minBound = X1_ 0
	maxBound = let a = X1_ (size a - 1) in a
	
type instance Index (X1_ a)  = Int
type instance Row (X1_ a)    = X1
type instance Column (X1_ a) = X1_ a

instance Size a => Size (X1_ a) where
	size = const s
	  where s = 2 * size (undefined :: a) + 1
	addIndex (X1_ v) n = X1_ (v + n)	-- fix bounds issues
	toIndex (X1_ v) = v
	seeIn2D (_,y) = y

type instance Index (X0_ a)  = Int
type instance Row (X0_ a)    = X1
type instance Column (X0_ a) = X0_ a

instance Size a => Bounded (X0_ a) where
	minBound = X0_ 0
	maxBound = let a = X0_ (size a - 1) in a

instance Size a => Size (X0_ a) where
	size = const s
	  where s = 2 * size (undefined :: a) 
	addIndex (X0_ v) n = X0_ (v + n)	-- fix bounds issues
	toIndex (X0_ v) = v
	seeIn2D (_,y) = y
	
------

type X1 = X1_ X0
type X2 = X0_ (X1_ X0)
type X3 = X1_ (X1_ X0)
type X4 = X0_ (X0_ (X1_ X0))
type X5 = X1_ (X0_ (X1_ X0))
type X6 = X0_ (X1_ (X1_ X0))
type X7 = X1_ (X1_ (X1_ X0))
type X8 = X0_ (X0_ (X0_ (X1_ X0)))
type X9 = X1_ (X0_ (X0_ (X1_ X0)))
type X10 = X0_ (X1_ (X0_ (X1_ X0)))
type X11 = X1_ (X1_ (X0_ (X1_ X0)))
type X12 = X0_ (X0_ (X1_ (X1_ X0)))
type X13 = X1_ (X0_ (X1_ (X1_ X0)))
type X14 = X0_ (X1_ (X1_ (X1_ X0)))
type X15 = X1_ (X1_ (X1_ (X1_ X0)))
type X16 = X0_ (X0_ (X0_ (X0_ (X1_ X0))))
type X17 = X1_ (X0_ (X0_ (X0_ (X1_ X0))))
type X18 = X0_ (X1_ (X0_ (X0_ (X1_ X0))))
type X19 = X1_ (X1_ (X0_ (X0_ (X1_ X0))))
type X20 = X0_ (X0_ (X1_ (X0_ (X1_ X0))))
type X21 = X1_ (X0_ (X1_ (X0_ (X1_ X0))))
type X22 = X0_ (X1_ (X1_ (X0_ (X1_ X0))))
type X23 = X1_ (X1_ (X1_ (X0_ (X1_ X0))))
type X24 = X0_ (X0_ (X0_ (X1_ (X1_ X0))))
type X25 = X1_ (X0_ (X0_ (X1_ (X1_ X0))))
type X26 = X0_ (X1_ (X0_ (X1_ (X1_ X0))))
type X27 = X1_ (X1_ (X0_ (X1_ (X1_ X0))))
type X28 = X0_ (X0_ (X1_ (X1_ (X1_ X0))))
type X29 = X1_ (X0_ (X1_ (X1_ (X1_ X0))))
type X30 = X0_ (X1_ (X1_ (X1_ (X1_ X0))))
type X31 = X1_ (X1_ (X1_ (X1_ (X1_ X0))))
type X32 = X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))
type X33 = X1_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))
type X34 = X0_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))
type X35 = X1_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))
type X36 = X0_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))
type X37 = X1_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))
type X38 = X0_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))
type X39 = X1_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))
type X40 = X0_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))
type X41 = X1_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))
type X42 = X0_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))
type X43 = X1_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))
type X44 = X0_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))
type X45 = X1_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))
type X46 = X0_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))
type X47 = X1_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))
type X48 = X0_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))
type X49 = X1_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))
type X50 = X0_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))
type X51 = X1_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))
type X52 = X0_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))
type X53 = X1_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))
type X54 = X0_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))
type X55 = X1_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))
type X56 = X0_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))
type X57 = X1_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))
type X58 = X0_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))
type X59 = X1_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))
type X60 = X0_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))
type X61 = X1_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))
type X62 = X0_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))
type X63 = X1_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))
type X64 = X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0))))))
type X65 = X1_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0))))))
type X66 = X0_ (X1_ (X0_ (X0_ (X0_ (X0_ (X1_ X0))))))
type X67 = X1_ (X1_ (X0_ (X0_ (X0_ (X0_ (X1_ X0))))))
type X68 = X0_ (X0_ (X1_ (X0_ (X0_ (X0_ (X1_ X0))))))
type X69 = X1_ (X0_ (X1_ (X0_ (X0_ (X0_ (X1_ X0))))))
type X70 = X0_ (X1_ (X1_ (X0_ (X0_ (X0_ (X1_ X0))))))
type X71 = X1_ (X1_ (X1_ (X0_ (X0_ (X0_ (X1_ X0))))))
type X72 = X0_ (X0_ (X0_ (X1_ (X0_ (X0_ (X1_ X0))))))
type X73 = X1_ (X0_ (X0_ (X1_ (X0_ (X0_ (X1_ X0))))))
type X74 = X0_ (X1_ (X0_ (X1_ (X0_ (X0_ (X1_ X0))))))
type X75 = X1_ (X1_ (X0_ (X1_ (X0_ (X0_ (X1_ X0))))))
type X76 = X0_ (X0_ (X1_ (X1_ (X0_ (X0_ (X1_ X0))))))
type X77 = X1_ (X0_ (X1_ (X1_ (X0_ (X0_ (X1_ X0))))))
type X78 = X0_ (X1_ (X1_ (X1_ (X0_ (X0_ (X1_ X0))))))
type X79 = X1_ (X1_ (X1_ (X1_ (X0_ (X0_ (X1_ X0))))))
type X80 = X0_ (X0_ (X0_ (X0_ (X1_ (X0_ (X1_ X0))))))
type X81 = X1_ (X0_ (X0_ (X0_ (X1_ (X0_ (X1_ X0))))))
type X82 = X0_ (X1_ (X0_ (X0_ (X1_ (X0_ (X1_ X0))))))
type X83 = X1_ (X1_ (X0_ (X0_ (X1_ (X0_ (X1_ X0))))))
type X84 = X0_ (X0_ (X1_ (X0_ (X1_ (X0_ (X1_ X0))))))
type X85 = X1_ (X0_ (X1_ (X0_ (X1_ (X0_ (X1_ X0))))))
type X86 = X0_ (X1_ (X1_ (X0_ (X1_ (X0_ (X1_ X0))))))
type X87 = X1_ (X1_ (X1_ (X0_ (X1_ (X0_ (X1_ X0))))))
type X88 = X0_ (X0_ (X0_ (X1_ (X1_ (X0_ (X1_ X0))))))
type X89 = X1_ (X0_ (X0_ (X1_ (X1_ (X0_ (X1_ X0))))))
type X90 = X0_ (X1_ (X0_ (X1_ (X1_ (X0_ (X1_ X0))))))
type X91 = X1_ (X1_ (X0_ (X1_ (X1_ (X0_ (X1_ X0))))))
type X92 = X0_ (X0_ (X1_ (X1_ (X1_ (X0_ (X1_ X0))))))
type X93 = X1_ (X0_ (X1_ (X1_ (X1_ (X0_ (X1_ X0))))))
type X94 = X0_ (X1_ (X1_ (X1_ (X1_ (X0_ (X1_ X0))))))
type X95 = X1_ (X1_ (X1_ (X1_ (X1_ (X0_ (X1_ X0))))))
type X96 = X0_ (X0_ (X0_ (X0_ (X0_ (X1_ (X1_ X0))))))
type X97 = X1_ (X0_ (X0_ (X0_ (X0_ (X1_ (X1_ X0))))))
type X98 = X0_ (X1_ (X0_ (X0_ (X0_ (X1_ (X1_ X0))))))
type X99 = X1_ (X1_ (X0_ (X0_ (X0_ (X1_ (X1_ X0))))))
type X100 = X0_ (X0_ (X1_ (X0_ (X0_ (X1_ (X1_ X0))))))
type X101 = X1_ (X0_ (X1_ (X0_ (X0_ (X1_ (X1_ X0))))))
type X102 = X0_ (X1_ (X1_ (X0_ (X0_ (X1_ (X1_ X0))))))
type X103 = X1_ (X1_ (X1_ (X0_ (X0_ (X1_ (X1_ X0))))))
type X104 = X0_ (X0_ (X0_ (X1_ (X0_ (X1_ (X1_ X0))))))
type X105 = X1_ (X0_ (X0_ (X1_ (X0_ (X1_ (X1_ X0))))))
type X106 = X0_ (X1_ (X0_ (X1_ (X0_ (X1_ (X1_ X0))))))
type X107 = X1_ (X1_ (X0_ (X1_ (X0_ (X1_ (X1_ X0))))))
type X108 = X0_ (X0_ (X1_ (X1_ (X0_ (X1_ (X1_ X0))))))
type X109 = X1_ (X0_ (X1_ (X1_ (X0_ (X1_ (X1_ X0))))))
type X110 = X0_ (X1_ (X1_ (X1_ (X0_ (X1_ (X1_ X0))))))
type X111 = X1_ (X1_ (X1_ (X1_ (X0_ (X1_ (X1_ X0))))))
type X112 = X0_ (X0_ (X0_ (X0_ (X1_ (X1_ (X1_ X0))))))
type X113 = X1_ (X0_ (X0_ (X0_ (X1_ (X1_ (X1_ X0))))))
type X114 = X0_ (X1_ (X0_ (X0_ (X1_ (X1_ (X1_ X0))))))
type X115 = X1_ (X1_ (X0_ (X0_ (X1_ (X1_ (X1_ X0))))))
type X116 = X0_ (X0_ (X1_ (X0_ (X1_ (X1_ (X1_ X0))))))
type X117 = X1_ (X0_ (X1_ (X0_ (X1_ (X1_ (X1_ X0))))))
type X118 = X0_ (X1_ (X1_ (X0_ (X1_ (X1_ (X1_ X0))))))
type X119 = X1_ (X1_ (X1_ (X0_ (X1_ (X1_ (X1_ X0))))))
type X120 = X0_ (X0_ (X0_ (X1_ (X1_ (X1_ (X1_ X0))))))
type X121 = X1_ (X0_ (X0_ (X1_ (X1_ (X1_ (X1_ X0))))))
type X122 = X0_ (X1_ (X0_ (X1_ (X1_ (X1_ (X1_ X0))))))
type X123 = X1_ (X1_ (X0_ (X1_ (X1_ (X1_ (X1_ X0))))))
type X124 = X0_ (X0_ (X1_ (X1_ (X1_ (X1_ (X1_ X0))))))
type X125 = X1_ (X0_ (X1_ (X1_ (X1_ (X1_ (X1_ X0))))))
type X126 = X0_ (X1_ (X1_ (X1_ (X1_ (X1_ (X1_ X0))))))
type X127 = X1_ (X1_ (X1_ (X1_ (X1_ (X1_ (X1_ X0))))))
type X128 = X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X129 = X1_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X130 = X0_ (X1_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X131 = X1_ (X1_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X132 = X0_ (X0_ (X1_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X133 = X1_ (X0_ (X1_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X134 = X0_ (X1_ (X1_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X135 = X1_ (X1_ (X1_ (X0_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X136 = X0_ (X0_ (X0_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X137 = X1_ (X0_ (X0_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X138 = X0_ (X1_ (X0_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X139 = X1_ (X1_ (X0_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X140 = X0_ (X0_ (X1_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X141 = X1_ (X0_ (X1_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X142 = X0_ (X1_ (X1_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X143 = X1_ (X1_ (X1_ (X1_ (X0_ (X0_ (X0_ (X1_ X0)))))))
type X144 = X0_ (X0_ (X0_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X145 = X1_ (X0_ (X0_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X146 = X0_ (X1_ (X0_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X147 = X1_ (X1_ (X0_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X148 = X0_ (X0_ (X1_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X149 = X1_ (X0_ (X1_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X150 = X0_ (X1_ (X1_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X151 = X1_ (X1_ (X1_ (X0_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X152 = X0_ (X0_ (X0_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X153 = X1_ (X0_ (X0_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X154 = X0_ (X1_ (X0_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X155 = X1_ (X1_ (X0_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X156 = X0_ (X0_ (X1_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X157 = X1_ (X0_ (X1_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X158 = X0_ (X1_ (X1_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X159 = X1_ (X1_ (X1_ (X1_ (X1_ (X0_ (X0_ (X1_ X0)))))))
type X160 = X0_ (X0_ (X0_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X161 = X1_ (X0_ (X0_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X162 = X0_ (X1_ (X0_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X163 = X1_ (X1_ (X0_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X164 = X0_ (X0_ (X1_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X165 = X1_ (X0_ (X1_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X166 = X0_ (X1_ (X1_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X167 = X1_ (X1_ (X1_ (X0_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X168 = X0_ (X0_ (X0_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X169 = X1_ (X0_ (X0_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X170 = X0_ (X1_ (X0_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X171 = X1_ (X1_ (X0_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X172 = X0_ (X0_ (X1_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X173 = X1_ (X0_ (X1_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X174 = X0_ (X1_ (X1_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X175 = X1_ (X1_ (X1_ (X1_ (X0_ (X1_ (X0_ (X1_ X0)))))))
type X176 = X0_ (X0_ (X0_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X177 = X1_ (X0_ (X0_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X178 = X0_ (X1_ (X0_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X179 = X1_ (X1_ (X0_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X180 = X0_ (X0_ (X1_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X181 = X1_ (X0_ (X1_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X182 = X0_ (X1_ (X1_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X183 = X1_ (X1_ (X1_ (X0_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X184 = X0_ (X0_ (X0_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X185 = X1_ (X0_ (X0_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X186 = X0_ (X1_ (X0_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X187 = X1_ (X1_ (X0_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X188 = X0_ (X0_ (X1_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X189 = X1_ (X0_ (X1_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X190 = X0_ (X1_ (X1_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X191 = X1_ (X1_ (X1_ (X1_ (X1_ (X1_ (X0_ (X1_ X0)))))))
type X192 = X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X193 = X1_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X194 = X0_ (X1_ (X0_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X195 = X1_ (X1_ (X0_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X196 = X0_ (X0_ (X1_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X197 = X1_ (X0_ (X1_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X198 = X0_ (X1_ (X1_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X199 = X1_ (X1_ (X1_ (X0_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X200 = X0_ (X0_ (X0_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X201 = X1_ (X0_ (X0_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X202 = X0_ (X1_ (X0_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X203 = X1_ (X1_ (X0_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X204 = X0_ (X0_ (X1_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X205 = X1_ (X0_ (X1_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X206 = X0_ (X1_ (X1_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X207 = X1_ (X1_ (X1_ (X1_ (X0_ (X0_ (X1_ (X1_ X0)))))))
type X208 = X0_ (X0_ (X0_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X209 = X1_ (X0_ (X0_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X210 = X0_ (X1_ (X0_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X211 = X1_ (X1_ (X0_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X212 = X0_ (X0_ (X1_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X213 = X1_ (X0_ (X1_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X214 = X0_ (X1_ (X1_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X215 = X1_ (X1_ (X1_ (X0_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X216 = X0_ (X0_ (X0_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X217 = X1_ (X0_ (X0_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X218 = X0_ (X1_ (X0_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X219 = X1_ (X1_ (X0_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X220 = X0_ (X0_ (X1_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X221 = X1_ (X0_ (X1_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X222 = X0_ (X1_ (X1_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X223 = X1_ (X1_ (X1_ (X1_ (X1_ (X0_ (X1_ (X1_ X0)))))))
type X224 = X0_ (X0_ (X0_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X225 = X1_ (X0_ (X0_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X226 = X0_ (X1_ (X0_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X227 = X1_ (X1_ (X0_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X228 = X0_ (X0_ (X1_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X229 = X1_ (X0_ (X1_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X230 = X0_ (X1_ (X1_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X231 = X1_ (X1_ (X1_ (X0_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X232 = X0_ (X0_ (X0_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X233 = X1_ (X0_ (X0_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X234 = X0_ (X1_ (X0_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X235 = X1_ (X1_ (X0_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X236 = X0_ (X0_ (X1_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X237 = X1_ (X0_ (X1_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X238 = X0_ (X1_ (X1_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X239 = X1_ (X1_ (X1_ (X1_ (X0_ (X1_ (X1_ (X1_ X0)))))))
type X240 = X0_ (X0_ (X0_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X241 = X1_ (X0_ (X0_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X242 = X0_ (X1_ (X0_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X243 = X1_ (X1_ (X0_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X244 = X0_ (X0_ (X1_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X245 = X1_ (X0_ (X1_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X246 = X0_ (X1_ (X1_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X247 = X1_ (X1_ (X1_ (X0_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X248 = X0_ (X0_ (X0_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X249 = X1_ (X0_ (X0_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X250 = X0_ (X1_ (X0_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X251 = X1_ (X1_ (X0_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X252 = X0_ (X0_ (X1_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X253 = X1_ (X0_ (X1_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X254 = X0_ (X1_ (X1_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X255 = X1_ (X1_ (X1_ (X1_ (X1_ (X1_ (X1_ (X1_ X0)))))))
type X256 = X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X0_ (X1_ X0))))))))
	