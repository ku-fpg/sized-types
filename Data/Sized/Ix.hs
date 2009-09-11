{-# LANGUAGE TemplateHaskell, TypeFamilies, EmptyDataDecls, UndecidableInstances, ScopedTypeVariables  #-}
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
	, X0_		-- for QC
	, X1_		-- for QC
	) where
	
import Language.Haskell.TH
import Data.Sized.Ix.TH
import Data.Ix


data a :# b

--- because of TH's lack of type families, will be added later.
type family Index a
type family Row a
type family Column a
type family ABS a
type family REP a

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
	seeIn2D = id

-- A good way of converting from one index type to another index type.
coerceSize :: (Index ix1 ~ Index ix2, Size ix1, Size ix2, Num ix2) => ix1 -> ix2
coerceSize ix = addIndex 0 (toIndex ix)

-- $(sizedTypeGenForUpto 256)
{-
type instance Index X0 = Int
type instance Index X1 = Int
type instance Index X2 = Int
type instance Index X3 = Int
type instance Index X4 = Int
type instance Index X5 = Int
type instance Index X6 = Int
type instance Index X7 = Int
type instance Index X8 = Int
type instance Index X9 = Int
type instance Index X10 = Int
type instance Index X11 = Int
type instance Index X12 = Int
type instance Index X13 = Int
type instance Index X14 = Int
type instance Index X15 = Int
type instance Index X16 = Int
type instance Index X17 = Int
type instance Index X18 = Int
type instance Index X19 = Int
type instance Index X20 = Int
type instance Index X21 = Int
type instance Index X22 = Int
type instance Index X23 = Int
type instance Index X24 = Int
type instance Index X25 = Int
type instance Index X26 = Int
type instance Index X27 = Int
type instance Index X28 = Int
type instance Index X29 = Int
type instance Index X30 = Int
type instance Index X31 = Int
type instance Index X32 = Int
type instance Index X33 = Int
type instance Index X34 = Int
type instance Index X35 = Int
type instance Index X36 = Int
type instance Index X37 = Int
type instance Index X38 = Int
type instance Index X39 = Int
type instance Index X40 = Int
type instance Index X41 = Int
type instance Index X42 = Int
type instance Index X43 = Int
type instance Index X44 = Int
type instance Index X45 = Int
type instance Index X46 = Int
type instance Index X47 = Int
type instance Index X48 = Int
type instance Index X49 = Int
type instance Index X50 = Int
type instance Index X51 = Int
type instance Index X52 = Int
type instance Index X53 = Int
type instance Index X54 = Int
type instance Index X55 = Int
type instance Index X56 = Int
type instance Index X57 = Int
type instance Index X58 = Int
type instance Index X59 = Int
type instance Index X60 = Int
type instance Index X61 = Int
type instance Index X62 = Int
type instance Index X63 = Int
type instance Index X64 = Int
type instance Index X65 = Int
type instance Index X66 = Int
type instance Index X67 = Int
type instance Index X68 = Int
type instance Index X69 = Int
type instance Index X70 = Int
type instance Index X71 = Int
type instance Index X72 = Int
type instance Index X73 = Int
type instance Index X74 = Int
type instance Index X75 = Int
type instance Index X76 = Int
type instance Index X77 = Int
type instance Index X78 = Int
type instance Index X79 = Int
type instance Index X80 = Int
type instance Index X81 = Int
type instance Index X82 = Int
type instance Index X83 = Int
type instance Index X84 = Int
type instance Index X85 = Int
type instance Index X86 = Int
type instance Index X87 = Int
type instance Index X88 = Int
type instance Index X89 = Int
type instance Index X90 = Int
type instance Index X91 = Int
type instance Index X92 = Int
type instance Index X93 = Int
type instance Index X94 = Int
type instance Index X95 = Int
type instance Index X96 = Int
type instance Index X97 = Int
type instance Index X98 = Int
type instance Index X99 = Int
type instance Index X100 = Int
type instance Index X101 = Int
type instance Index X102 = Int
type instance Index X103 = Int
type instance Index X104 = Int
type instance Index X105 = Int
type instance Index X106 = Int
type instance Index X107 = Int
type instance Index X108 = Int
type instance Index X109 = Int
type instance Index X110 = Int
type instance Index X111 = Int
type instance Index X112 = Int
type instance Index X113 = Int
type instance Index X114 = Int
type instance Index X115 = Int
type instance Index X116 = Int
type instance Index X117 = Int
type instance Index X118 = Int
type instance Index X119 = Int
type instance Index X120 = Int
type instance Index X121 = Int
type instance Index X122 = Int
type instance Index X123 = Int
type instance Index X124 = Int
type instance Index X125 = Int
type instance Index X126 = Int
type instance Index X127 = Int
type instance Index X128 = Int
type instance Index X129 = Int
type instance Index X130 = Int
type instance Index X131 = Int
type instance Index X132 = Int
type instance Index X133 = Int
type instance Index X134 = Int
type instance Index X135 = Int
type instance Index X136 = Int
type instance Index X137 = Int
type instance Index X138 = Int
type instance Index X139 = Int
type instance Index X140 = Int
type instance Index X141 = Int
type instance Index X142 = Int
type instance Index X143 = Int
type instance Index X144 = Int
type instance Index X145 = Int
type instance Index X146 = Int
type instance Index X147 = Int
type instance Index X148 = Int
type instance Index X149 = Int
type instance Index X150 = Int
type instance Index X151 = Int
type instance Index X152 = Int
type instance Index X153 = Int
type instance Index X154 = Int
type instance Index X155 = Int
type instance Index X156 = Int
type instance Index X157 = Int
type instance Index X158 = Int
type instance Index X159 = Int
type instance Index X160 = Int
type instance Index X161 = Int
type instance Index X162 = Int
type instance Index X163 = Int
type instance Index X164 = Int
type instance Index X165 = Int
type instance Index X166 = Int
type instance Index X167 = Int
type instance Index X168 = Int
type instance Index X169 = Int
type instance Index X170 = Int
type instance Index X171 = Int
type instance Index X172 = Int
type instance Index X173 = Int
type instance Index X174 = Int
type instance Index X175 = Int
type instance Index X176 = Int
type instance Index X177 = Int
type instance Index X178 = Int
type instance Index X179 = Int
type instance Index X180 = Int
type instance Index X181 = Int
type instance Index X182 = Int
type instance Index X183 = Int
type instance Index X184 = Int
type instance Index X185 = Int
type instance Index X186 = Int
type instance Index X187 = Int
type instance Index X188 = Int
type instance Index X189 = Int
type instance Index X190 = Int
type instance Index X191 = Int
type instance Index X192 = Int
type instance Index X193 = Int
type instance Index X194 = Int
type instance Index X195 = Int
type instance Index X196 = Int
type instance Index X197 = Int
type instance Index X198 = Int
type instance Index X199 = Int
type instance Index X200 = Int
type instance Index X201 = Int
type instance Index X202 = Int
type instance Index X203 = Int
type instance Index X204 = Int
type instance Index X205 = Int
type instance Index X206 = Int
type instance Index X207 = Int
type instance Index X208 = Int
type instance Index X209 = Int
type instance Index X210 = Int
type instance Index X211 = Int
type instance Index X212 = Int
type instance Index X213 = Int
type instance Index X214 = Int
type instance Index X215 = Int
type instance Index X216 = Int
type instance Index X217 = Int
type instance Index X218 = Int
type instance Index X219 = Int
type instance Index X220 = Int
type instance Index X221 = Int
type instance Index X222 = Int
type instance Index X223 = Int
type instance Index X224 = Int
type instance Index X225 = Int
type instance Index X226 = Int
type instance Index X227 = Int
type instance Index X228 = Int
type instance Index X229 = Int
type instance Index X230 = Int
type instance Index X231 = Int
type instance Index X232 = Int
type instance Index X233 = Int
type instance Index X234 = Int
type instance Index X235 = Int
type instance Index X236 = Int
type instance Index X237 = Int
type instance Index X238 = Int
type instance Index X239 = Int
type instance Index X240 = Int
type instance Index X241 = Int
type instance Index X242 = Int
type instance Index X243 = Int
type instance Index X244 = Int
type instance Index X245 = Int
type instance Index X246 = Int
type instance Index X247 = Int
type instance Index X248 = Int
type instance Index X249 = Int
type instance Index X250 = Int
type instance Index X251 = Int
type instance Index X252 = Int
type instance Index X253 = Int
type instance Index X254 = Int
type instance Index X255 = Int
type instance Index X256 = Int

type instance Row X0 = X1
type instance Row X1 = X1
type instance Row X2 = X1
type instance Row X3 = X1
type instance Row X4 = X1
type instance Row X5 = X1
type instance Row X6 = X1
type instance Row X7 = X1
type instance Row X8 = X1
type instance Row X9 = X1
type instance Row X10 = X1
type instance Row X11 = X1
type instance Row X12 = X1
type instance Row X13 = X1
type instance Row X14 = X1
type instance Row X15 = X1
type instance Row X16 = X1
type instance Row X17 = X1
type instance Row X18 = X1
type instance Row X19 = X1
type instance Row X20 = X1
type instance Row X21 = X1
type instance Row X22 = X1
type instance Row X23 = X1
type instance Row X24 = X1
type instance Row X25 = X1
type instance Row X26 = X1
type instance Row X27 = X1
type instance Row X28 = X1
type instance Row X29 = X1
type instance Row X30 = X1
type instance Row X31 = X1
type instance Row X32 = X1
type instance Row X33 = X1
type instance Row X34 = X1
type instance Row X35 = X1
type instance Row X36 = X1
type instance Row X37 = X1
type instance Row X38 = X1
type instance Row X39 = X1
type instance Row X40 = X1
type instance Row X41 = X1
type instance Row X42 = X1
type instance Row X43 = X1
type instance Row X44 = X1
type instance Row X45 = X1
type instance Row X46 = X1
type instance Row X47 = X1
type instance Row X48 = X1
type instance Row X49 = X1
type instance Row X50 = X1
type instance Row X51 = X1
type instance Row X52 = X1
type instance Row X53 = X1
type instance Row X54 = X1
type instance Row X55 = X1
type instance Row X56 = X1
type instance Row X57 = X1
type instance Row X58 = X1
type instance Row X59 = X1
type instance Row X60 = X1
type instance Row X61 = X1
type instance Row X62 = X1
type instance Row X63 = X1
type instance Row X64 = X1
type instance Row X65 = X1
type instance Row X66 = X1
type instance Row X67 = X1
type instance Row X68 = X1
type instance Row X69 = X1
type instance Row X70 = X1
type instance Row X71 = X1
type instance Row X72 = X1
type instance Row X73 = X1
type instance Row X74 = X1
type instance Row X75 = X1
type instance Row X76 = X1
type instance Row X77 = X1
type instance Row X78 = X1
type instance Row X79 = X1
type instance Row X80 = X1
type instance Row X81 = X1
type instance Row X82 = X1
type instance Row X83 = X1
type instance Row X84 = X1
type instance Row X85 = X1
type instance Row X86 = X1
type instance Row X87 = X1
type instance Row X88 = X1
type instance Row X89 = X1
type instance Row X90 = X1
type instance Row X91 = X1
type instance Row X92 = X1
type instance Row X93 = X1
type instance Row X94 = X1
type instance Row X95 = X1
type instance Row X96 = X1
type instance Row X97 = X1
type instance Row X98 = X1
type instance Row X99 = X1
type instance Row X100 = X1
type instance Row X101 = X1
type instance Row X102 = X1
type instance Row X103 = X1
type instance Row X104 = X1
type instance Row X105 = X1
type instance Row X106 = X1
type instance Row X107 = X1
type instance Row X108 = X1
type instance Row X109 = X1
type instance Row X110 = X1
type instance Row X111 = X1
type instance Row X112 = X1
type instance Row X113 = X1
type instance Row X114 = X1
type instance Row X115 = X1
type instance Row X116 = X1
type instance Row X117 = X1
type instance Row X118 = X1
type instance Row X119 = X1
type instance Row X120 = X1
type instance Row X121 = X1
type instance Row X122 = X1
type instance Row X123 = X1
type instance Row X124 = X1
type instance Row X125 = X1
type instance Row X126 = X1
type instance Row X127 = X1
type instance Row X128 = X1
type instance Row X129 = X1
type instance Row X130 = X1
type instance Row X131 = X1
type instance Row X132 = X1
type instance Row X133 = X1
type instance Row X134 = X1
type instance Row X135 = X1
type instance Row X136 = X1
type instance Row X137 = X1
type instance Row X138 = X1
type instance Row X139 = X1
type instance Row X140 = X1
type instance Row X141 = X1
type instance Row X142 = X1
type instance Row X143 = X1
type instance Row X144 = X1
type instance Row X145 = X1
type instance Row X146 = X1
type instance Row X147 = X1
type instance Row X148 = X1
type instance Row X149 = X1
type instance Row X150 = X1
type instance Row X151 = X1
type instance Row X152 = X1
type instance Row X153 = X1
type instance Row X154 = X1
type instance Row X155 = X1
type instance Row X156 = X1
type instance Row X157 = X1
type instance Row X158 = X1
type instance Row X159 = X1
type instance Row X160 = X1
type instance Row X161 = X1
type instance Row X162 = X1
type instance Row X163 = X1
type instance Row X164 = X1
type instance Row X165 = X1
type instance Row X166 = X1
type instance Row X167 = X1
type instance Row X168 = X1
type instance Row X169 = X1
type instance Row X170 = X1
type instance Row X171 = X1
type instance Row X172 = X1
type instance Row X173 = X1
type instance Row X174 = X1
type instance Row X175 = X1
type instance Row X176 = X1
type instance Row X177 = X1
type instance Row X178 = X1
type instance Row X179 = X1
type instance Row X180 = X1
type instance Row X181 = X1
type instance Row X182 = X1
type instance Row X183 = X1
type instance Row X184 = X1
type instance Row X185 = X1
type instance Row X186 = X1
type instance Row X187 = X1
type instance Row X188 = X1
type instance Row X189 = X1
type instance Row X190 = X1
type instance Row X191 = X1
type instance Row X192 = X1
type instance Row X193 = X1
type instance Row X194 = X1
type instance Row X195 = X1
type instance Row X196 = X1
type instance Row X197 = X1
type instance Row X198 = X1
type instance Row X199 = X1
type instance Row X200 = X1
type instance Row X201 = X1
type instance Row X202 = X1
type instance Row X203 = X1
type instance Row X204 = X1
type instance Row X205 = X1
type instance Row X206 = X1
type instance Row X207 = X1
type instance Row X208 = X1
type instance Row X209 = X1
type instance Row X210 = X1
type instance Row X211 = X1
type instance Row X212 = X1
type instance Row X213 = X1
type instance Row X214 = X1
type instance Row X215 = X1
type instance Row X216 = X1
type instance Row X217 = X1
type instance Row X218 = X1
type instance Row X219 = X1
type instance Row X220 = X1
type instance Row X221 = X1
type instance Row X222 = X1
type instance Row X223 = X1
type instance Row X224 = X1
type instance Row X225 = X1
type instance Row X226 = X1
type instance Row X227 = X1
type instance Row X228 = X1
type instance Row X229 = X1
type instance Row X230 = X1
type instance Row X231 = X1
type instance Row X232 = X1
type instance Row X233 = X1
type instance Row X234 = X1
type instance Row X235 = X1
type instance Row X236 = X1
type instance Row X237 = X1
type instance Row X238 = X1
type instance Row X239 = X1
type instance Row X240 = X1
type instance Row X241 = X1
type instance Row X242 = X1
type instance Row X243 = X1
type instance Row X244 = X1
type instance Row X245 = X1
type instance Row X246 = X1
type instance Row X247 = X1
type instance Row X248 = X1
type instance Row X249 = X1
type instance Row X250 = X1
type instance Row X251 = X1
type instance Row X252 = X1
type instance Row X253 = X1
type instance Row X254 = X1
type instance Row X255 = X1
type instance Row X256 = X1

type instance Column X0 = X0
type instance Column X1 = X1
type instance Column X2 = X2
type instance Column X3 = X3
type instance Column X4 = X4
type instance Column X5 = X5
type instance Column X6 = X6
type instance Column X7 = X7
type instance Column X8 = X8
type instance Column X9 = X9
type instance Column X10 = X10
type instance Column X11 = X11
type instance Column X12 = X12
type instance Column X13 = X13
type instance Column X14 = X14
type instance Column X15 = X15
type instance Column X16 = X16
type instance Column X17 = X17
type instance Column X18 = X18
type instance Column X19 = X19
type instance Column X20 = X20
type instance Column X21 = X21
type instance Column X22 = X22
type instance Column X23 = X23
type instance Column X24 = X24
type instance Column X25 = X25
type instance Column X26 = X26
type instance Column X27 = X27
type instance Column X28 = X28
type instance Column X29 = X29
type instance Column X30 = X30
type instance Column X31 = X31
type instance Column X32 = X32
type instance Column X33 = X33
type instance Column X34 = X34
type instance Column X35 = X35
type instance Column X36 = X36
type instance Column X37 = X37
type instance Column X38 = X38
type instance Column X39 = X39
type instance Column X40 = X40
type instance Column X41 = X41
type instance Column X42 = X42
type instance Column X43 = X43
type instance Column X44 = X44
type instance Column X45 = X45
type instance Column X46 = X46
type instance Column X47 = X47
type instance Column X48 = X48
type instance Column X49 = X49
type instance Column X50 = X50
type instance Column X51 = X51
type instance Column X52 = X52
type instance Column X53 = X53
type instance Column X54 = X54
type instance Column X55 = X55
type instance Column X56 = X56
type instance Column X57 = X57
type instance Column X58 = X58
type instance Column X59 = X59
type instance Column X60 = X60
type instance Column X61 = X61
type instance Column X62 = X62
type instance Column X63 = X63
type instance Column X64 = X64
type instance Column X65 = X65
type instance Column X66 = X66
type instance Column X67 = X67
type instance Column X68 = X68
type instance Column X69 = X69
type instance Column X70 = X70
type instance Column X71 = X71
type instance Column X72 = X72
type instance Column X73 = X73
type instance Column X74 = X74
type instance Column X75 = X75
type instance Column X76 = X76
type instance Column X77 = X77
type instance Column X78 = X78
type instance Column X79 = X79
type instance Column X80 = X80
type instance Column X81 = X81
type instance Column X82 = X82
type instance Column X83 = X83
type instance Column X84 = X84
type instance Column X85 = X85
type instance Column X86 = X86
type instance Column X87 = X87
type instance Column X88 = X88
type instance Column X89 = X89
type instance Column X90 = X90
type instance Column X91 = X91
type instance Column X92 = X92
type instance Column X93 = X93
type instance Column X94 = X94
type instance Column X95 = X95
type instance Column X96 = X96
type instance Column X97 = X97
type instance Column X98 = X98
type instance Column X99 = X99
type instance Column X100 = X100
type instance Column X101 = X101
type instance Column X102 = X102
type instance Column X103 = X103
type instance Column X104 = X104
type instance Column X105 = X105
type instance Column X106 = X106
type instance Column X107 = X107
type instance Column X108 = X108
type instance Column X109 = X109
type instance Column X110 = X110
type instance Column X111 = X111
type instance Column X112 = X112
type instance Column X113 = X113
type instance Column X114 = X114
type instance Column X115 = X115
type instance Column X116 = X116
type instance Column X117 = X117
type instance Column X118 = X118
type instance Column X119 = X119
type instance Column X120 = X120
type instance Column X121 = X121
type instance Column X122 = X122
type instance Column X123 = X123
type instance Column X124 = X124
type instance Column X125 = X125
type instance Column X126 = X126
type instance Column X127 = X127
type instance Column X128 = X128
type instance Column X129 = X129
type instance Column X130 = X130
type instance Column X131 = X131
type instance Column X132 = X132
type instance Column X133 = X133
type instance Column X134 = X134
type instance Column X135 = X135
type instance Column X136 = X136
type instance Column X137 = X137
type instance Column X138 = X138
type instance Column X139 = X139
type instance Column X140 = X140
type instance Column X141 = X141
type instance Column X142 = X142
type instance Column X143 = X143
type instance Column X144 = X144
type instance Column X145 = X145
type instance Column X146 = X146
type instance Column X147 = X147
type instance Column X148 = X148
type instance Column X149 = X149
type instance Column X150 = X150
type instance Column X151 = X151
type instance Column X152 = X152
type instance Column X153 = X153
type instance Column X154 = X154
type instance Column X155 = X155
type instance Column X156 = X156
type instance Column X157 = X157
type instance Column X158 = X158
type instance Column X159 = X159
type instance Column X160 = X160
type instance Column X161 = X161
type instance Column X162 = X162
type instance Column X163 = X163
type instance Column X164 = X164
type instance Column X165 = X165
type instance Column X166 = X166
type instance Column X167 = X167
type instance Column X168 = X168
type instance Column X169 = X169
type instance Column X170 = X170
type instance Column X171 = X171
type instance Column X172 = X172
type instance Column X173 = X173
type instance Column X174 = X174
type instance Column X175 = X175
type instance Column X176 = X176
type instance Column X177 = X177
type instance Column X178 = X178
type instance Column X179 = X179
type instance Column X180 = X180
type instance Column X181 = X181
type instance Column X182 = X182
type instance Column X183 = X183
type instance Column X184 = X184
type instance Column X185 = X185
type instance Column X186 = X186
type instance Column X187 = X187
type instance Column X188 = X188
type instance Column X189 = X189
type instance Column X190 = X190
type instance Column X191 = X191
type instance Column X192 = X192
type instance Column X193 = X193
type instance Column X194 = X194
type instance Column X195 = X195
type instance Column X196 = X196
type instance Column X197 = X197
type instance Column X198 = X198
type instance Column X199 = X199
type instance Column X200 = X200
type instance Column X201 = X201
type instance Column X202 = X202
type instance Column X203 = X203
type instance Column X204 = X204
type instance Column X205 = X205
type instance Column X206 = X206
type instance Column X207 = X207
type instance Column X208 = X208
type instance Column X209 = X209
type instance Column X210 = X210
type instance Column X211 = X211
type instance Column X212 = X212
type instance Column X213 = X213
type instance Column X214 = X214
type instance Column X215 = X215
type instance Column X216 = X216
type instance Column X217 = X217
type instance Column X218 = X218
type instance Column X219 = X219
type instance Column X220 = X220
type instance Column X221 = X221
type instance Column X222 = X222
type instance Column X223 = X223
type instance Column X224 = X224
type instance Column X225 = X225
type instance Column X226 = X226
type instance Column X227 = X227
type instance Column X228 = X228
type instance Column X229 = X229
type instance Column X230 = X230
type instance Column X231 = X231
type instance Column X232 = X232
type instance Column X233 = X233
type instance Column X234 = X234
type instance Column X235 = X235
type instance Column X236 = X236
type instance Column X237 = X237
type instance Column X238 = X238
type instance Column X239 = X239
type instance Column X240 = X240
type instance Column X241 = X241
type instance Column X242 = X242
type instance Column X243 = X243
type instance Column X244 = X244
type instance Column X245 = X245
type instance Column X246 = X246
type instance Column X247 = X247
type instance Column X248 = X248
type instance Column X249 = X249
type instance Column X250 = X250
type instance Column X251 = X251
type instance Column X252 = X252
type instance Column X253 = X253
type instance Column X254 = X254
type instance Column X255 = X255
type instance Column X256 = X256
{-
type instance REP X0 = X0
type instance REP X1 = X1
type instance REP X2 = (X1,X0)
type instance REP X3 = (X1,X1)
type instance REP X4 = ((X1,X0),X0)
type instance REP X5 = ((X1,X0),X1)
type instance REP X6 = ((X1,X1),X0)
type instance REP X7 = ((X1,X1),X1)
type instance REP X8 = (((X1,X0),X0),X0)
type instance REP X9 = (((X1,X0),X0),X1)
type instance REP X10 = (((X1,X0),X1),X0)
type instance REP X11 = (((X1,X0),X1),X1)
type instance REP X12 = (((X1,X1),X0),X0)
type instance REP X13 = (((X1,X1),X0),X1)
type instance REP X14 = (((X1,X1),X1),X0)
type instance REP X15 = (((X1,X1),X1),X1)
type instance REP X16 = ((((X1,X0),X0),X0),X0)
type instance REP X17 = ((((X1,X0),X0),X0),X1)
type instance REP X18 = ((((X1,X0),X0),X1),X0)
type instance REP X19 = ((((X1,X0),X0),X1),X1)
type instance REP X20 = ((((X1,X0),X1),X0),X0)
type instance REP X21 = ((((X1,X0),X1),X0),X1)
type instance REP X22 = ((((X1,X0),X1),X1),X0)
type instance REP X23 = ((((X1,X0),X1),X1),X1)
type instance REP X24 = ((((X1,X1),X0),X0),X0)
type instance REP X25 = ((((X1,X1),X0),X0),X1)
type instance REP X26 = ((((X1,X1),X0),X1),X0)
type instance REP X27 = ((((X1,X1),X0),X1),X1)
type instance REP X28 = ((((X1,X1),X1),X0),X0)
type instance REP X29 = ((((X1,X1),X1),X0),X1)
type instance REP X30 = ((((X1,X1),X1),X1),X0)
type instance REP X31 = ((((X1,X1),X1),X1),X1)
type instance REP X32 = (((((X1,X0),X0),X0),X0),X0)
type instance REP X33 = (((((X1,X0),X0),X0),X0),X1)
type instance REP X34 = (((((X1,X0),X0),X0),X1),X0)
type instance REP X35 = (((((X1,X0),X0),X0),X1),X1)
type instance REP X36 = (((((X1,X0),X0),X1),X0),X0)
type instance REP X37 = (((((X1,X0),X0),X1),X0),X1)
type instance REP X38 = (((((X1,X0),X0),X1),X1),X0)
type instance REP X39 = (((((X1,X0),X0),X1),X1),X1)
type instance REP X40 = (((((X1,X0),X1),X0),X0),X0)
type instance REP X41 = (((((X1,X0),X1),X0),X0),X1)
type instance REP X42 = (((((X1,X0),X1),X0),X1),X0)
type instance REP X43 = (((((X1,X0),X1),X0),X1),X1)
type instance REP X44 = (((((X1,X0),X1),X1),X0),X0)
type instance REP X45 = (((((X1,X0),X1),X1),X0),X1)
type instance REP X46 = (((((X1,X0),X1),X1),X1),X0)
type instance REP X47 = (((((X1,X0),X1),X1),X1),X1)
type instance REP X48 = (((((X1,X1),X0),X0),X0),X0)
type instance REP X49 = (((((X1,X1),X0),X0),X0),X1)
type instance REP X50 = (((((X1,X1),X0),X0),X1),X0)
type instance REP X51 = (((((X1,X1),X0),X0),X1),X1)
type instance REP X52 = (((((X1,X1),X0),X1),X0),X0)
type instance REP X53 = (((((X1,X1),X0),X1),X0),X1)
type instance REP X54 = (((((X1,X1),X0),X1),X1),X0)
type instance REP X55 = (((((X1,X1),X0),X1),X1),X1)
type instance REP X56 = (((((X1,X1),X1),X0),X0),X0)
type instance REP X57 = (((((X1,X1),X1),X0),X0),X1)
type instance REP X58 = (((((X1,X1),X1),X0),X1),X0)
type instance REP X59 = (((((X1,X1),X1),X0),X1),X1)
type instance REP X60 = (((((X1,X1),X1),X1),X0),X0)
type instance REP X61 = (((((X1,X1),X1),X1),X0),X1)
type instance REP X62 = (((((X1,X1),X1),X1),X1),X0)
type instance REP X63 = (((((X1,X1),X1),X1),X1),X1)
type instance REP X64 = ((((((X1,X0),X0),X0),X0),X0),X0)
type instance REP X65 = ((((((X1,X0),X0),X0),X0),X0),X1)
type instance REP X66 = ((((((X1,X0),X0),X0),X0),X1),X0)
type instance REP X67 = ((((((X1,X0),X0),X0),X0),X1),X1)
type instance REP X68 = ((((((X1,X0),X0),X0),X1),X0),X0)
type instance REP X69 = ((((((X1,X0),X0),X0),X1),X0),X1)
type instance REP X70 = ((((((X1,X0),X0),X0),X1),X1),X0)
type instance REP X71 = ((((((X1,X0),X0),X0),X1),X1),X1)
type instance REP X72 = ((((((X1,X0),X0),X1),X0),X0),X0)
type instance REP X73 = ((((((X1,X0),X0),X1),X0),X0),X1)
type instance REP X74 = ((((((X1,X0),X0),X1),X0),X1),X0)
type instance REP X75 = ((((((X1,X0),X0),X1),X0),X1),X1)
type instance REP X76 = ((((((X1,X0),X0),X1),X1),X0),X0)
type instance REP X77 = ((((((X1,X0),X0),X1),X1),X0),X1)
type instance REP X78 = ((((((X1,X0),X0),X1),X1),X1),X0)
type instance REP X79 = ((((((X1,X0),X0),X1),X1),X1),X1)
type instance REP X80 = ((((((X1,X0),X1),X0),X0),X0),X0)
type instance REP X81 = ((((((X1,X0),X1),X0),X0),X0),X1)
type instance REP X82 = ((((((X1,X0),X1),X0),X0),X1),X0)
type instance REP X83 = ((((((X1,X0),X1),X0),X0),X1),X1)
type instance REP X84 = ((((((X1,X0),X1),X0),X1),X0),X0)
type instance REP X85 = ((((((X1,X0),X1),X0),X1),X0),X1)
type instance REP X86 = ((((((X1,X0),X1),X0),X1),X1),X0)
type instance REP X87 = ((((((X1,X0),X1),X0),X1),X1),X1)
type instance REP X88 = ((((((X1,X0),X1),X1),X0),X0),X0)
type instance REP X89 = ((((((X1,X0),X1),X1),X0),X0),X1)
type instance REP X90 = ((((((X1,X0),X1),X1),X0),X1),X0)
type instance REP X91 = ((((((X1,X0),X1),X1),X0),X1),X1)
type instance REP X92 = ((((((X1,X0),X1),X1),X1),X0),X0)
type instance REP X93 = ((((((X1,X0),X1),X1),X1),X0),X1)
type instance REP X94 = ((((((X1,X0),X1),X1),X1),X1),X0)
type instance REP X95 = ((((((X1,X0),X1),X1),X1),X1),X1)
type instance REP X96 = ((((((X1,X1),X0),X0),X0),X0),X0)
type instance REP X97 = ((((((X1,X1),X0),X0),X0),X0),X1)
type instance REP X98 = ((((((X1,X1),X0),X0),X0),X1),X0)
type instance REP X99 = ((((((X1,X1),X0),X0),X0),X1),X1)
type instance REP X100 = ((((((X1,X1),X0),X0),X1),X0),X0)
type instance REP X101 = ((((((X1,X1),X0),X0),X1),X0),X1)
type instance REP X102 = ((((((X1,X1),X0),X0),X1),X1),X0)
type instance REP X103 = ((((((X1,X1),X0),X0),X1),X1),X1)
type instance REP X104 = ((((((X1,X1),X0),X1),X0),X0),X0)
type instance REP X105 = ((((((X1,X1),X0),X1),X0),X0),X1)
type instance REP X106 = ((((((X1,X1),X0),X1),X0),X1),X0)
type instance REP X107 = ((((((X1,X1),X0),X1),X0),X1),X1)
type instance REP X108 = ((((((X1,X1),X0),X1),X1),X0),X0)
type instance REP X109 = ((((((X1,X1),X0),X1),X1),X0),X1)
type instance REP X110 = ((((((X1,X1),X0),X1),X1),X1),X0)
type instance REP X111 = ((((((X1,X1),X0),X1),X1),X1),X1)
type instance REP X112 = ((((((X1,X1),X1),X0),X0),X0),X0)
type instance REP X113 = ((((((X1,X1),X1),X0),X0),X0),X1)
type instance REP X114 = ((((((X1,X1),X1),X0),X0),X1),X0)
type instance REP X115 = ((((((X1,X1),X1),X0),X0),X1),X1)
type instance REP X116 = ((((((X1,X1),X1),X0),X1),X0),X0)
type instance REP X117 = ((((((X1,X1),X1),X0),X1),X0),X1)
type instance REP X118 = ((((((X1,X1),X1),X0),X1),X1),X0)
type instance REP X119 = ((((((X1,X1),X1),X0),X1),X1),X1)
type instance REP X120 = ((((((X1,X1),X1),X1),X0),X0),X0)
type instance REP X121 = ((((((X1,X1),X1),X1),X0),X0),X1)
type instance REP X122 = ((((((X1,X1),X1),X1),X0),X1),X0)
type instance REP X123 = ((((((X1,X1),X1),X1),X0),X1),X1)
type instance REP X124 = ((((((X1,X1),X1),X1),X1),X0),X0)
type instance REP X125 = ((((((X1,X1),X1),X1),X1),X0),X1)
type instance REP X126 = ((((((X1,X1),X1),X1),X1),X1),X0)
type instance REP X127 = ((((((X1,X1),X1),X1),X1),X1),X1)
type instance REP X128 = (((((((X1,X0),X0),X0),X0),X0),X0),X0)
type instance REP X129 = (((((((X1,X0),X0),X0),X0),X0),X0),X1)
type instance REP X130 = (((((((X1,X0),X0),X0),X0),X0),X1),X0)
type instance REP X131 = (((((((X1,X0),X0),X0),X0),X0),X1),X1)
type instance REP X132 = (((((((X1,X0),X0),X0),X0),X1),X0),X0)
type instance REP X133 = (((((((X1,X0),X0),X0),X0),X1),X0),X1)
type instance REP X134 = (((((((X1,X0),X0),X0),X0),X1),X1),X0)
type instance REP X135 = (((((((X1,X0),X0),X0),X0),X1),X1),X1)
type instance REP X136 = (((((((X1,X0),X0),X0),X1),X0),X0),X0)
type instance REP X137 = (((((((X1,X0),X0),X0),X1),X0),X0),X1)
type instance REP X138 = (((((((X1,X0),X0),X0),X1),X0),X1),X0)
type instance REP X139 = (((((((X1,X0),X0),X0),X1),X0),X1),X1)
type instance REP X140 = (((((((X1,X0),X0),X0),X1),X1),X0),X0)
type instance REP X141 = (((((((X1,X0),X0),X0),X1),X1),X0),X1)
type instance REP X142 = (((((((X1,X0),X0),X0),X1),X1),X1),X0)
type instance REP X143 = (((((((X1,X0),X0),X0),X1),X1),X1),X1)
type instance REP X144 = (((((((X1,X0),X0),X1),X0),X0),X0),X0)
type instance REP X145 = (((((((X1,X0),X0),X1),X0),X0),X0),X1)
type instance REP X146 = (((((((X1,X0),X0),X1),X0),X0),X1),X0)
type instance REP X147 = (((((((X1,X0),X0),X1),X0),X0),X1),X1)
type instance REP X148 = (((((((X1,X0),X0),X1),X0),X1),X0),X0)
type instance REP X149 = (((((((X1,X0),X0),X1),X0),X1),X0),X1)
type instance REP X150 = (((((((X1,X0),X0),X1),X0),X1),X1),X0)
type instance REP X151 = (((((((X1,X0),X0),X1),X0),X1),X1),X1)
type instance REP X152 = (((((((X1,X0),X0),X1),X1),X0),X0),X0)
type instance REP X153 = (((((((X1,X0),X0),X1),X1),X0),X0),X1)
type instance REP X154 = (((((((X1,X0),X0),X1),X1),X0),X1),X0)
type instance REP X155 = (((((((X1,X0),X0),X1),X1),X0),X1),X1)
type instance REP X156 = (((((((X1,X0),X0),X1),X1),X1),X0),X0)
type instance REP X157 = (((((((X1,X0),X0),X1),X1),X1),X0),X1)
type instance REP X158 = (((((((X1,X0),X0),X1),X1),X1),X1),X0)
type instance REP X159 = (((((((X1,X0),X0),X1),X1),X1),X1),X1)
type instance REP X160 = (((((((X1,X0),X1),X0),X0),X0),X0),X0)
type instance REP X161 = (((((((X1,X0),X1),X0),X0),X0),X0),X1)
type instance REP X162 = (((((((X1,X0),X1),X0),X0),X0),X1),X0)
type instance REP X163 = (((((((X1,X0),X1),X0),X0),X0),X1),X1)
type instance REP X164 = (((((((X1,X0),X1),X0),X0),X1),X0),X0)
type instance REP X165 = (((((((X1,X0),X1),X0),X0),X1),X0),X1)
type instance REP X166 = (((((((X1,X0),X1),X0),X0),X1),X1),X0)
type instance REP X167 = (((((((X1,X0),X1),X0),X0),X1),X1),X1)
type instance REP X168 = (((((((X1,X0),X1),X0),X1),X0),X0),X0)
type instance REP X169 = (((((((X1,X0),X1),X0),X1),X0),X0),X1)
type instance REP X170 = (((((((X1,X0),X1),X0),X1),X0),X1),X0)
type instance REP X171 = (((((((X1,X0),X1),X0),X1),X0),X1),X1)
type instance REP X172 = (((((((X1,X0),X1),X0),X1),X1),X0),X0)
type instance REP X173 = (((((((X1,X0),X1),X0),X1),X1),X0),X1)
type instance REP X174 = (((((((X1,X0),X1),X0),X1),X1),X1),X0)
type instance REP X175 = (((((((X1,X0),X1),X0),X1),X1),X1),X1)
type instance REP X176 = (((((((X1,X0),X1),X1),X0),X0),X0),X0)
type instance REP X177 = (((((((X1,X0),X1),X1),X0),X0),X0),X1)
type instance REP X178 = (((((((X1,X0),X1),X1),X0),X0),X1),X0)
type instance REP X179 = (((((((X1,X0),X1),X1),X0),X0),X1),X1)
type instance REP X180 = (((((((X1,X0),X1),X1),X0),X1),X0),X0)
type instance REP X181 = (((((((X1,X0),X1),X1),X0),X1),X0),X1)
type instance REP X182 = (((((((X1,X0),X1),X1),X0),X1),X1),X0)
type instance REP X183 = (((((((X1,X0),X1),X1),X0),X1),X1),X1)
type instance REP X184 = (((((((X1,X0),X1),X1),X1),X0),X0),X0)
type instance REP X185 = (((((((X1,X0),X1),X1),X1),X0),X0),X1)
type instance REP X186 = (((((((X1,X0),X1),X1),X1),X0),X1),X0)
type instance REP X187 = (((((((X1,X0),X1),X1),X1),X0),X1),X1)
type instance REP X188 = (((((((X1,X0),X1),X1),X1),X1),X0),X0)
type instance REP X189 = (((((((X1,X0),X1),X1),X1),X1),X0),X1)
type instance REP X190 = (((((((X1,X0),X1),X1),X1),X1),X1),X0)
type instance REP X191 = (((((((X1,X0),X1),X1),X1),X1),X1),X1)
type instance REP X192 = (((((((X1,X1),X0),X0),X0),X0),X0),X0)
type instance REP X193 = (((((((X1,X1),X0),X0),X0),X0),X0),X1)
type instance REP X194 = (((((((X1,X1),X0),X0),X0),X0),X1),X0)
type instance REP X195 = (((((((X1,X1),X0),X0),X0),X0),X1),X1)
type instance REP X196 = (((((((X1,X1),X0),X0),X0),X1),X0),X0)
type instance REP X197 = (((((((X1,X1),X0),X0),X0),X1),X0),X1)
type instance REP X198 = (((((((X1,X1),X0),X0),X0),X1),X1),X0)
type instance REP X199 = (((((((X1,X1),X0),X0),X0),X1),X1),X1)
type instance REP X200 = (((((((X1,X1),X0),X0),X1),X0),X0),X0)
type instance REP X201 = (((((((X1,X1),X0),X0),X1),X0),X0),X1)
type instance REP X202 = (((((((X1,X1),X0),X0),X1),X0),X1),X0)
type instance REP X203 = (((((((X1,X1),X0),X0),X1),X0),X1),X1)
type instance REP X204 = (((((((X1,X1),X0),X0),X1),X1),X0),X0)
type instance REP X205 = (((((((X1,X1),X0),X0),X1),X1),X0),X1)
type instance REP X206 = (((((((X1,X1),X0),X0),X1),X1),X1),X0)
type instance REP X207 = (((((((X1,X1),X0),X0),X1),X1),X1),X1)
type instance REP X208 = (((((((X1,X1),X0),X1),X0),X0),X0),X0)
type instance REP X209 = (((((((X1,X1),X0),X1),X0),X0),X0),X1)
type instance REP X210 = (((((((X1,X1),X0),X1),X0),X0),X1),X0)
type instance REP X211 = (((((((X1,X1),X0),X1),X0),X0),X1),X1)
type instance REP X212 = (((((((X1,X1),X0),X1),X0),X1),X0),X0)
type instance REP X213 = (((((((X1,X1),X0),X1),X0),X1),X0),X1)
type instance REP X214 = (((((((X1,X1),X0),X1),X0),X1),X1),X0)
type instance REP X215 = (((((((X1,X1),X0),X1),X0),X1),X1),X1)
type instance REP X216 = (((((((X1,X1),X0),X1),X1),X0),X0),X0)
type instance REP X217 = (((((((X1,X1),X0),X1),X1),X0),X0),X1)
type instance REP X218 = (((((((X1,X1),X0),X1),X1),X0),X1),X0)
type instance REP X219 = (((((((X1,X1),X0),X1),X1),X0),X1),X1)
type instance REP X220 = (((((((X1,X1),X0),X1),X1),X1),X0),X0)
type instance REP X221 = (((((((X1,X1),X0),X1),X1),X1),X0),X1)
type instance REP X222 = (((((((X1,X1),X0),X1),X1),X1),X1),X0)
type instance REP X223 = (((((((X1,X1),X0),X1),X1),X1),X1),X1)
type instance REP X224 = (((((((X1,X1),X1),X0),X0),X0),X0),X0)
type instance REP X225 = (((((((X1,X1),X1),X0),X0),X0),X0),X1)
type instance REP X226 = (((((((X1,X1),X1),X0),X0),X0),X1),X0)
type instance REP X227 = (((((((X1,X1),X1),X0),X0),X0),X1),X1)
type instance REP X228 = (((((((X1,X1),X1),X0),X0),X1),X0),X0)
type instance REP X229 = (((((((X1,X1),X1),X0),X0),X1),X0),X1)
type instance REP X230 = (((((((X1,X1),X1),X0),X0),X1),X1),X0)
type instance REP X231 = (((((((X1,X1),X1),X0),X0),X1),X1),X1)
type instance REP X232 = (((((((X1,X1),X1),X0),X1),X0),X0),X0)
type instance REP X233 = (((((((X1,X1),X1),X0),X1),X0),X0),X1)
type instance REP X234 = (((((((X1,X1),X1),X0),X1),X0),X1),X0)
type instance REP X235 = (((((((X1,X1),X1),X0),X1),X0),X1),X1)
type instance REP X236 = (((((((X1,X1),X1),X0),X1),X1),X0),X0)
type instance REP X237 = (((((((X1,X1),X1),X0),X1),X1),X0),X1)
type instance REP X238 = (((((((X1,X1),X1),X0),X1),X1),X1),X0)
type instance REP X239 = (((((((X1,X1),X1),X0),X1),X1),X1),X1)
type instance REP X240 = (((((((X1,X1),X1),X1),X0),X0),X0),X0)
type instance REP X241 = (((((((X1,X1),X1),X1),X0),X0),X0),X1)
type instance REP X242 = (((((((X1,X1),X1),X1),X0),X0),X1),X0)
type instance REP X243 = (((((((X1,X1),X1),X1),X0),X0),X1),X1)
type instance REP X244 = (((((((X1,X1),X1),X1),X0),X1),X0),X0)
type instance REP X245 = (((((((X1,X1),X1),X1),X0),X1),X0),X1)
type instance REP X246 = (((((((X1,X1),X1),X1),X0),X1),X1),X0)
type instance REP X247 = (((((((X1,X1),X1),X1),X0),X1),X1),X1)
type instance REP X248 = (((((((X1,X1),X1),X1),X1),X0),X0),X0)
type instance REP X249 = (((((((X1,X1),X1),X1),X1),X0),X0),X1)
type instance REP X250 = (((((((X1,X1),X1),X1),X1),X0),X1),X0)
type instance REP X251 = (((((((X1,X1),X1),X1),X1),X0),X1),X1)
type instance REP X252 = (((((((X1,X1),X1),X1),X1),X1),X0),X0)
type instance REP X253 = (((((((X1,X1),X1),X1),X1),X1),X0),X1)
type instance REP X254 = (((((((X1,X1),X1),X1),X1),X1),X1),X0)
type instance REP X255 = (((((((X1,X1),X1),X1),X1),X1),X1),X1)
type instance REP X256 = ((((((((X1,X0),X0),X0),X0),X0),X0),X0),X0)
type instance ABS X0 = X0
type instance ABS X1 = X1
type instance ABS (X1,X0) = X2
type instance ABS (X1,X1) = X3
type instance ABS ((X1,X0),X0) = X4
type instance ABS ((X1,X0),X1) = X5
type instance ABS ((X1,X1),X0) = X6
type instance ABS ((X1,X1),X1) = X7
type instance ABS (((X1,X0),X0),X0) = X8
type instance ABS (((X1,X0),X0),X1) = X9
type instance ABS (((X1,X0),X1),X0) = X10
type instance ABS (((X1,X0),X1),X1) = X11
type instance ABS (((X1,X1),X0),X0) = X12
type instance ABS (((X1,X1),X0),X1) = X13
type instance ABS (((X1,X1),X1),X0) = X14
type instance ABS (((X1,X1),X1),X1) = X15
type instance ABS ((((X1,X0),X0),X0),X0) = X16
type instance ABS ((((X1,X0),X0),X0),X1) = X17
type instance ABS ((((X1,X0),X0),X1),X0) = X18
type instance ABS ((((X1,X0),X0),X1),X1) = X19
type instance ABS ((((X1,X0),X1),X0),X0) = X20
type instance ABS ((((X1,X0),X1),X0),X1) = X21
type instance ABS ((((X1,X0),X1),X1),X0) = X22
type instance ABS ((((X1,X0),X1),X1),X1) = X23
type instance ABS ((((X1,X1),X0),X0),X0) = X24
type instance ABS ((((X1,X1),X0),X0),X1) = X25
type instance ABS ((((X1,X1),X0),X1),X0) = X26
type instance ABS ((((X1,X1),X0),X1),X1) = X27
type instance ABS ((((X1,X1),X1),X0),X0) = X28
type instance ABS ((((X1,X1),X1),X0),X1) = X29
type instance ABS ((((X1,X1),X1),X1),X0) = X30
type instance ABS ((((X1,X1),X1),X1),X1) = X31
type instance ABS (((((X1,X0),X0),X0),X0),X0) = X32
type instance ABS (((((X1,X0),X0),X0),X0),X1) = X33
type instance ABS (((((X1,X0),X0),X0),X1),X0) = X34
type instance ABS (((((X1,X0),X0),X0),X1),X1) = X35
type instance ABS (((((X1,X0),X0),X1),X0),X0) = X36
type instance ABS (((((X1,X0),X0),X1),X0),X1) = X37
type instance ABS (((((X1,X0),X0),X1),X1),X0) = X38
type instance ABS (((((X1,X0),X0),X1),X1),X1) = X39
type instance ABS (((((X1,X0),X1),X0),X0),X0) = X40
type instance ABS (((((X1,X0),X1),X0),X0),X1) = X41
type instance ABS (((((X1,X0),X1),X0),X1),X0) = X42
type instance ABS (((((X1,X0),X1),X0),X1),X1) = X43
type instance ABS (((((X1,X0),X1),X1),X0),X0) = X44
type instance ABS (((((X1,X0),X1),X1),X0),X1) = X45
type instance ABS (((((X1,X0),X1),X1),X1),X0) = X46
type instance ABS (((((X1,X0),X1),X1),X1),X1) = X47
type instance ABS (((((X1,X1),X0),X0),X0),X0) = X48
type instance ABS (((((X1,X1),X0),X0),X0),X1) = X49
type instance ABS (((((X1,X1),X0),X0),X1),X0) = X50
type instance ABS (((((X1,X1),X0),X0),X1),X1) = X51
type instance ABS (((((X1,X1),X0),X1),X0),X0) = X52
type instance ABS (((((X1,X1),X0),X1),X0),X1) = X53
type instance ABS (((((X1,X1),X0),X1),X1),X0) = X54
type instance ABS (((((X1,X1),X0),X1),X1),X1) = X55
type instance ABS (((((X1,X1),X1),X0),X0),X0) = X56
type instance ABS (((((X1,X1),X1),X0),X0),X1) = X57
type instance ABS (((((X1,X1),X1),X0),X1),X0) = X58
type instance ABS (((((X1,X1),X1),X0),X1),X1) = X59
type instance ABS (((((X1,X1),X1),X1),X0),X0) = X60
type instance ABS (((((X1,X1),X1),X1),X0),X1) = X61
type instance ABS (((((X1,X1),X1),X1),X1),X0) = X62
type instance ABS (((((X1,X1),X1),X1),X1),X1) = X63
type instance ABS ((((((X1,X0),X0),X0),X0),X0),X0) = X64
type instance ABS ((((((X1,X0),X0),X0),X0),X0),X1) = X65
type instance ABS ((((((X1,X0),X0),X0),X0),X1),X0) = X66
type instance ABS ((((((X1,X0),X0),X0),X0),X1),X1) = X67
type instance ABS ((((((X1,X0),X0),X0),X1),X0),X0) = X68
type instance ABS ((((((X1,X0),X0),X0),X1),X0),X1) = X69
type instance ABS ((((((X1,X0),X0),X0),X1),X1),X0) = X70
type instance ABS ((((((X1,X0),X0),X0),X1),X1),X1) = X71
type instance ABS ((((((X1,X0),X0),X1),X0),X0),X0) = X72
type instance ABS ((((((X1,X0),X0),X1),X0),X0),X1) = X73
type instance ABS ((((((X1,X0),X0),X1),X0),X1),X0) = X74
type instance ABS ((((((X1,X0),X0),X1),X0),X1),X1) = X75
type instance ABS ((((((X1,X0),X0),X1),X1),X0),X0) = X76
type instance ABS ((((((X1,X0),X0),X1),X1),X0),X1) = X77
type instance ABS ((((((X1,X0),X0),X1),X1),X1),X0) = X78
type instance ABS ((((((X1,X0),X0),X1),X1),X1),X1) = X79
type instance ABS ((((((X1,X0),X1),X0),X0),X0),X0) = X80
type instance ABS ((((((X1,X0),X1),X0),X0),X0),X1) = X81
type instance ABS ((((((X1,X0),X1),X0),X0),X1),X0) = X82
type instance ABS ((((((X1,X0),X1),X0),X0),X1),X1) = X83
type instance ABS ((((((X1,X0),X1),X0),X1),X0),X0) = X84
type instance ABS ((((((X1,X0),X1),X0),X1),X0),X1) = X85
type instance ABS ((((((X1,X0),X1),X0),X1),X1),X0) = X86
type instance ABS ((((((X1,X0),X1),X0),X1),X1),X1) = X87
type instance ABS ((((((X1,X0),X1),X1),X0),X0),X0) = X88
type instance ABS ((((((X1,X0),X1),X1),X0),X0),X1) = X89
type instance ABS ((((((X1,X0),X1),X1),X0),X1),X0) = X90
type instance ABS ((((((X1,X0),X1),X1),X0),X1),X1) = X91
type instance ABS ((((((X1,X0),X1),X1),X1),X0),X0) = X92
type instance ABS ((((((X1,X0),X1),X1),X1),X0),X1) = X93
type instance ABS ((((((X1,X0),X1),X1),X1),X1),X0) = X94
type instance ABS ((((((X1,X0),X1),X1),X1),X1),X1) = X95
type instance ABS ((((((X1,X1),X0),X0),X0),X0),X0) = X96
type instance ABS ((((((X1,X1),X0),X0),X0),X0),X1) = X97
type instance ABS ((((((X1,X1),X0),X0),X0),X1),X0) = X98
type instance ABS ((((((X1,X1),X0),X0),X0),X1),X1) = X99
type instance ABS ((((((X1,X1),X0),X0),X1),X0),X0) = X100
type instance ABS ((((((X1,X1),X0),X0),X1),X0),X1) = X101
type instance ABS ((((((X1,X1),X0),X0),X1),X1),X0) = X102
type instance ABS ((((((X1,X1),X0),X0),X1),X1),X1) = X103
type instance ABS ((((((X1,X1),X0),X1),X0),X0),X0) = X104
type instance ABS ((((((X1,X1),X0),X1),X0),X0),X1) = X105
type instance ABS ((((((X1,X1),X0),X1),X0),X1),X0) = X106
type instance ABS ((((((X1,X1),X0),X1),X0),X1),X1) = X107
type instance ABS ((((((X1,X1),X0),X1),X1),X0),X0) = X108
type instance ABS ((((((X1,X1),X0),X1),X1),X0),X1) = X109
type instance ABS ((((((X1,X1),X0),X1),X1),X1),X0) = X110
type instance ABS ((((((X1,X1),X0),X1),X1),X1),X1) = X111
type instance ABS ((((((X1,X1),X1),X0),X0),X0),X0) = X112
type instance ABS ((((((X1,X1),X1),X0),X0),X0),X1) = X113
type instance ABS ((((((X1,X1),X1),X0),X0),X1),X0) = X114
type instance ABS ((((((X1,X1),X1),X0),X0),X1),X1) = X115
type instance ABS ((((((X1,X1),X1),X0),X1),X0),X0) = X116
type instance ABS ((((((X1,X1),X1),X0),X1),X0),X1) = X117
type instance ABS ((((((X1,X1),X1),X0),X1),X1),X0) = X118
type instance ABS ((((((X1,X1),X1),X0),X1),X1),X1) = X119
type instance ABS ((((((X1,X1),X1),X1),X0),X0),X0) = X120
type instance ABS ((((((X1,X1),X1),X1),X0),X0),X1) = X121
type instance ABS ((((((X1,X1),X1),X1),X0),X1),X0) = X122
type instance ABS ((((((X1,X1),X1),X1),X0),X1),X1) = X123
type instance ABS ((((((X1,X1),X1),X1),X1),X0),X0) = X124
type instance ABS ((((((X1,X1),X1),X1),X1),X0),X1) = X125
type instance ABS ((((((X1,X1),X1),X1),X1),X1),X0) = X126
type instance ABS ((((((X1,X1),X1),X1),X1),X1),X1) = X127
type instance ABS (((((((X1,X0),X0),X0),X0),X0),X0),X0) = X128
type instance ABS (((((((X1,X0),X0),X0),X0),X0),X0),X1) = X129
type instance ABS (((((((X1,X0),X0),X0),X0),X0),X1),X0) = X130
type instance ABS (((((((X1,X0),X0),X0),X0),X0),X1),X1) = X131
type instance ABS (((((((X1,X0),X0),X0),X0),X1),X0),X0) = X132
type instance ABS (((((((X1,X0),X0),X0),X0),X1),X0),X1) = X133
type instance ABS (((((((X1,X0),X0),X0),X0),X1),X1),X0) = X134
type instance ABS (((((((X1,X0),X0),X0),X0),X1),X1),X1) = X135
type instance ABS (((((((X1,X0),X0),X0),X1),X0),X0),X0) = X136
type instance ABS (((((((X1,X0),X0),X0),X1),X0),X0),X1) = X137
type instance ABS (((((((X1,X0),X0),X0),X1),X0),X1),X0) = X138
type instance ABS (((((((X1,X0),X0),X0),X1),X0),X1),X1) = X139
type instance ABS (((((((X1,X0),X0),X0),X1),X1),X0),X0) = X140
type instance ABS (((((((X1,X0),X0),X0),X1),X1),X0),X1) = X141
type instance ABS (((((((X1,X0),X0),X0),X1),X1),X1),X0) = X142
type instance ABS (((((((X1,X0),X0),X0),X1),X1),X1),X1) = X143
type instance ABS (((((((X1,X0),X0),X1),X0),X0),X0),X0) = X144
type instance ABS (((((((X1,X0),X0),X1),X0),X0),X0),X1) = X145
type instance ABS (((((((X1,X0),X0),X1),X0),X0),X1),X0) = X146
type instance ABS (((((((X1,X0),X0),X1),X0),X0),X1),X1) = X147
type instance ABS (((((((X1,X0),X0),X1),X0),X1),X0),X0) = X148
type instance ABS (((((((X1,X0),X0),X1),X0),X1),X0),X1) = X149
type instance ABS (((((((X1,X0),X0),X1),X0),X1),X1),X0) = X150
type instance ABS (((((((X1,X0),X0),X1),X0),X1),X1),X1) = X151
type instance ABS (((((((X1,X0),X0),X1),X1),X0),X0),X0) = X152
type instance ABS (((((((X1,X0),X0),X1),X1),X0),X0),X1) = X153
type instance ABS (((((((X1,X0),X0),X1),X1),X0),X1),X0) = X154
type instance ABS (((((((X1,X0),X0),X1),X1),X0),X1),X1) = X155
type instance ABS (((((((X1,X0),X0),X1),X1),X1),X0),X0) = X156
type instance ABS (((((((X1,X0),X0),X1),X1),X1),X0),X1) = X157
type instance ABS (((((((X1,X0),X0),X1),X1),X1),X1),X0) = X158
type instance ABS (((((((X1,X0),X0),X1),X1),X1),X1),X1) = X159
type instance ABS (((((((X1,X0),X1),X0),X0),X0),X0),X0) = X160
type instance ABS (((((((X1,X0),X1),X0),X0),X0),X0),X1) = X161
type instance ABS (((((((X1,X0),X1),X0),X0),X0),X1),X0) = X162
type instance ABS (((((((X1,X0),X1),X0),X0),X0),X1),X1) = X163
type instance ABS (((((((X1,X0),X1),X0),X0),X1),X0),X0) = X164
type instance ABS (((((((X1,X0),X1),X0),X0),X1),X0),X1) = X165
type instance ABS (((((((X1,X0),X1),X0),X0),X1),X1),X0) = X166
type instance ABS (((((((X1,X0),X1),X0),X0),X1),X1),X1) = X167
type instance ABS (((((((X1,X0),X1),X0),X1),X0),X0),X0) = X168
type instance ABS (((((((X1,X0),X1),X0),X1),X0),X0),X1) = X169
type instance ABS (((((((X1,X0),X1),X0),X1),X0),X1),X0) = X170
type instance ABS (((((((X1,X0),X1),X0),X1),X0),X1),X1) = X171
type instance ABS (((((((X1,X0),X1),X0),X1),X1),X0),X0) = X172
type instance ABS (((((((X1,X0),X1),X0),X1),X1),X0),X1) = X173
type instance ABS (((((((X1,X0),X1),X0),X1),X1),X1),X0) = X174
type instance ABS (((((((X1,X0),X1),X0),X1),X1),X1),X1) = X175
type instance ABS (((((((X1,X0),X1),X1),X0),X0),X0),X0) = X176
type instance ABS (((((((X1,X0),X1),X1),X0),X0),X0),X1) = X177
type instance ABS (((((((X1,X0),X1),X1),X0),X0),X1),X0) = X178
type instance ABS (((((((X1,X0),X1),X1),X0),X0),X1),X1) = X179
type instance ABS (((((((X1,X0),X1),X1),X0),X1),X0),X0) = X180
type instance ABS (((((((X1,X0),X1),X1),X0),X1),X0),X1) = X181
type instance ABS (((((((X1,X0),X1),X1),X0),X1),X1),X0) = X182
type instance ABS (((((((X1,X0),X1),X1),X0),X1),X1),X1) = X183
type instance ABS (((((((X1,X0),X1),X1),X1),X0),X0),X0) = X184
type instance ABS (((((((X1,X0),X1),X1),X1),X0),X0),X1) = X185
type instance ABS (((((((X1,X0),X1),X1),X1),X0),X1),X0) = X186
type instance ABS (((((((X1,X0),X1),X1),X1),X0),X1),X1) = X187
type instance ABS (((((((X1,X0),X1),X1),X1),X1),X0),X0) = X188
type instance ABS (((((((X1,X0),X1),X1),X1),X1),X0),X1) = X189
type instance ABS (((((((X1,X0),X1),X1),X1),X1),X1),X0) = X190
type instance ABS (((((((X1,X0),X1),X1),X1),X1),X1),X1) = X191
type instance ABS (((((((X1,X1),X0),X0),X0),X0),X0),X0) = X192
type instance ABS (((((((X1,X1),X0),X0),X0),X0),X0),X1) = X193
type instance ABS (((((((X1,X1),X0),X0),X0),X0),X1),X0) = X194
type instance ABS (((((((X1,X1),X0),X0),X0),X0),X1),X1) = X195
type instance ABS (((((((X1,X1),X0),X0),X0),X1),X0),X0) = X196
type instance ABS (((((((X1,X1),X0),X0),X0),X1),X0),X1) = X197
type instance ABS (((((((X1,X1),X0),X0),X0),X1),X1),X0) = X198
type instance ABS (((((((X1,X1),X0),X0),X0),X1),X1),X1) = X199
type instance ABS (((((((X1,X1),X0),X0),X1),X0),X0),X0) = X200
type instance ABS (((((((X1,X1),X0),X0),X1),X0),X0),X1) = X201
type instance ABS (((((((X1,X1),X0),X0),X1),X0),X1),X0) = X202
type instance ABS (((((((X1,X1),X0),X0),X1),X0),X1),X1) = X203
type instance ABS (((((((X1,X1),X0),X0),X1),X1),X0),X0) = X204
type instance ABS (((((((X1,X1),X0),X0),X1),X1),X0),X1) = X205
type instance ABS (((((((X1,X1),X0),X0),X1),X1),X1),X0) = X206
type instance ABS (((((((X1,X1),X0),X0),X1),X1),X1),X1) = X207
type instance ABS (((((((X1,X1),X0),X1),X0),X0),X0),X0) = X208
type instance ABS (((((((X1,X1),X0),X1),X0),X0),X0),X1) = X209
type instance ABS (((((((X1,X1),X0),X1),X0),X0),X1),X0) = X210
type instance ABS (((((((X1,X1),X0),X1),X0),X0),X1),X1) = X211
type instance ABS (((((((X1,X1),X0),X1),X0),X1),X0),X0) = X212
type instance ABS (((((((X1,X1),X0),X1),X0),X1),X0),X1) = X213
type instance ABS (((((((X1,X1),X0),X1),X0),X1),X1),X0) = X214
type instance ABS (((((((X1,X1),X0),X1),X0),X1),X1),X1) = X215
type instance ABS (((((((X1,X1),X0),X1),X1),X0),X0),X0) = X216
type instance ABS (((((((X1,X1),X0),X1),X1),X0),X0),X1) = X217
type instance ABS (((((((X1,X1),X0),X1),X1),X0),X1),X0) = X218
type instance ABS (((((((X1,X1),X0),X1),X1),X0),X1),X1) = X219
type instance ABS (((((((X1,X1),X0),X1),X1),X1),X0),X0) = X220
type instance ABS (((((((X1,X1),X0),X1),X1),X1),X0),X1) = X221
type instance ABS (((((((X1,X1),X0),X1),X1),X1),X1),X0) = X222
type instance ABS (((((((X1,X1),X0),X1),X1),X1),X1),X1) = X223
type instance ABS (((((((X1,X1),X1),X0),X0),X0),X0),X0) = X224
type instance ABS (((((((X1,X1),X1),X0),X0),X0),X0),X1) = X225
type instance ABS (((((((X1,X1),X1),X0),X0),X0),X1),X0) = X226
type instance ABS (((((((X1,X1),X1),X0),X0),X0),X1),X1) = X227
type instance ABS (((((((X1,X1),X1),X0),X0),X1),X0),X0) = X228
type instance ABS (((((((X1,X1),X1),X0),X0),X1),X0),X1) = X229
type instance ABS (((((((X1,X1),X1),X0),X0),X1),X1),X0) = X230
type instance ABS (((((((X1,X1),X1),X0),X0),X1),X1),X1) = X231
type instance ABS (((((((X1,X1),X1),X0),X1),X0),X0),X0) = X232
type instance ABS (((((((X1,X1),X1),X0),X1),X0),X0),X1) = X233
type instance ABS (((((((X1,X1),X1),X0),X1),X0),X1),X0) = X234
type instance ABS (((((((X1,X1),X1),X0),X1),X0),X1),X1) = X235
type instance ABS (((((((X1,X1),X1),X0),X1),X1),X0),X0) = X236
type instance ABS (((((((X1,X1),X1),X0),X1),X1),X0),X1) = X237
type instance ABS (((((((X1,X1),X1),X0),X1),X1),X1),X0) = X238
type instance ABS (((((((X1,X1),X1),X0),X1),X1),X1),X1) = X239
type instance ABS (((((((X1,X1),X1),X1),X0),X0),X0),X0) = X240
type instance ABS (((((((X1,X1),X1),X1),X0),X0),X0),X1) = X241
type instance ABS (((((((X1,X1),X1),X1),X0),X0),X1),X0) = X242
type instance ABS (((((((X1,X1),X1),X1),X0),X0),X1),X1) = X243
type instance ABS (((((((X1,X1),X1),X1),X0),X1),X0),X0) = X244
type instance ABS (((((((X1,X1),X1),X1),X0),X1),X0),X1) = X245
type instance ABS (((((((X1,X1),X1),X1),X0),X1),X1),X0) = X246
type instance ABS (((((((X1,X1),X1),X1),X0),X1),X1),X1) = X247
type instance ABS (((((((X1,X1),X1),X1),X1),X0),X0),X0) = X248
type instance ABS (((((((X1,X1),X1),X1),X1),X0),X0),X1) = X249
type instance ABS (((((((X1,X1),X1),X1),X1),X0),X1),X0) = X250
type instance ABS (((((((X1,X1),X1),X1),X1),X0),X1),X1) = X251
type instance ABS (((((((X1,X1),X1),X1),X1),X1),X0),X0) = X252
type instance ABS (((((((X1,X1),X1),X1),X1),X1),X0),X1) = X253
type instance ABS (((((((X1,X1),X1),X1),X1),X1),X1),X0) = X254
type instance ABS (((((((X1,X1),X1),X1),X1),X1),X1),X1) = X255
type instance ABS ((((((((X1,X0),X0),X0),X0),X0),X0),X0),X0) = X256
-}

-- Now, arithmetic

data X0_ a	-- big endian
data X1_ a
data NEG1

type family ADD a b
type instance ADD NEG1 NEG1 = NEG1
type instance ADD NEG1 X0   = NEG1
type instance ADD NEG1 (X0_ b) = X1
type instance ADD NEG1 (X1_ a) = X1
type instance ADD X0   NEG1 = NEG1
type instance ADD X0   X1   = X1
type instance ADD X0   X1   = X1
type instance ADD X1   NEG1 = X0
type instance ADD X1   X0   = X1
type instance ADD X1   X1   = X1
type instance ADD X1 X0   = X1
type instance ADD X1 X1   = X0_ X1

typ

type instance ADD X0 (a,b) = (a,b)
type instance ADD X1 (a,X0) = (a,X1)
type instance ADD X1 (a,X1) = (ADD a X1,X0)
type instance ADD (a,b) X0 = (a,b)
type instance ADD (a,X0) X1 = (a,X1)
type instance ADD (a,X1) X1 = (ADD a X1,X0)
type instance ADD (a,X0) (b,X0)  = (ADD a b,X0)
type instance ADD (a,X0) (b,X1)  = (ADD a b,X1)
type instance ADD (a,X1) (b,X0)  = (ADD a b,X1)
type instance ADD (a,X1) (b,X1)  = (ADD (ADD a b) X1,X0)


data UNDEFINED

type instance SUB X0 X0 = X0
type instance SUB X1 X0 = X1
type instance SUB X0 X1 = UNDEFINED
type instance SUB X1 X1 = X0

type instance SUB X0 (a,b) = UNDEFINED
type instance SUB X1 (a,b) = UNDEFINED

type instance SUB (a,b)  X0 = (a,b)
type instance SUB (a,X0) X1 = NORM (SUB a X1,X1) -- two instances
type instance SUB (a,X1) X1 = (a,X0)
type instance SUB (a,X0) (b,X0) = (SUB a b,X0)
type instance SUB (a,X0) (b,X1) = NORM (SUB a (ADD b X1),X1)
type instance SUB (a,X1) (b,X0) = (SUB a b,X1)
type instance SUB (a,X1) (b,X1) = (SUB a b,X0)


--- SUBSHIFTADD a b == 2 * (a - b) + 1
type family NORM a b
type instance NORM X0    = X1
type instance NORM X1    = (X1,X1)
type instance NORM (X0,b) = (NORM (a,b),X1)
type instance NORM (X0,b) = (NORM (a,b),X1)


type instance SUBSHIFTADD X0 X0 = X1
type instance SUBSHIFTADD X0 X1 = UNDEFINED
type instance SUBSHIFTADD X1 X0 = (X1,X1)
type instance SUBSHIFTADD X1 X1 = X1
type instance SUBSHIFTADD (a,b) X0 = X1


type instance SUB X0 (a,X0) = UNDEFINED
type instance ADD (a,b) X0 = (a,b)
type instance ADD (a,X0) X1 = (a,X1)
type instance ADD (a,X1) X1 = (ADD a X1,X0)
type instance ADD (a,X0) (b,X0)  = (ADD a b,X0)
type instance ADD (a,X0) (b,X1)  = (ADD a b,X1)
type instance ADD (a,X1) (b,X0)  = (ADD a b,X1)
type instance ADD (a,X1) (b,X1)  = (ADD (ADD a b) X1,X0)


type instance SUB (a,X1) X0 = (ADD a X1,X0)
type instance SUB X1 (a,X1) = (ADD a X1,X0)
type instance SUB X0 (a,b) = UNDEFINED


type Times2 a = ABS (REP a,X0)

type Add a b = ABS (ADD (REP a) (REP b))
-}

data X0_ a = X0_ Int

instance Eq (X0_ a) where
	(X0_ a) == (X0_ b) = a == b

instance Ord (X0_ a) where
	(X0_ a) `compare` (X0_ b) = a `compare` b

instance Size a => Bounded (X0_ a) where
	minBound = X0_ 0
	maxBound = let a = X0_ (size a - 1) in a

type instance Index (X0_ a)  = Int
type instance Row (X0_ a)    = D1
type instance Column (X0_ a) = X0_ a

instance Size a => Size (X0_ a) where
	size = const s
	  where s = 2 * size (undefined :: a) 
	addIndex (X0_ v) n = X0_ (v + n)	-- fix bounds issues
	toIndex (X0_ v) = v
	seeIn2D = snd

instance Ix (X0_ a) where
	range (X0_ a,X0_ b) = map X0_ (range (a,b))
	index (X0_ a,X0_ b) (X0_ i) = index (a,b) i
	inRange (X0_ a,X0_ b) (X0_ i) = inRange (a,b) i

instance Enum (X0_ a) where
	toEnum n = (X0_ n)
	fromEnum (X0_ n) = n

instance Show (X0_ a) where
	show (X0_ a) = show a

data X1_ a = X1_ Int
	
instance Eq (X1_ a) where
	(X1_ a) == (X1_ b) = a == b

instance Ord (X1_ a) where
	(X1_ a) `compare` (X1_ b) = a `compare` b

instance Size a => Bounded (X1_ a) where
	minBound = X1_ 0
	maxBound = let a = X1_ (size a - 1) in a

type instance Index (X1_ a)  = Int
type instance Row (X1_ a)    = D1
type instance Column (X1_ a) = X1_ a

instance Size a => Size (X1_ a) where
	size = const s
	  where s = 2 * size (undefined :: a) + 1
	addIndex (X1_ v) n = X1_ (v + n)	-- fix bounds issues
	toIndex (X1_ v) = v
	seeIn2D = snd

instance Ix (X1_ a) where
	range (X1_ a,X1_ b) = map X1_ (range (a,b))
	index (X1_ a,X1_ b) (X1_ i) = index (a,b) i
	inRange (X1_ a,X1_ b) (X1_ i) = inRange (a,b) i

instance Enum (X1_ a) where
	toEnum n = (X1_ n)
	fromEnum (X1_ n) = n

instance Show (X1_ a) where
	show (X1_ a) = show a

data N1

data X0 = X0
	deriving (Eq,Ord)

instance Bounded X0 where
	minBound = error "minBound not defined"
	maxBound = error "maxBound not defined"

type instance Index X0  = Int
type instance Row X0    = D1
type instance Column X0 = X0

instance Size X0 where
	size _ = 0
	addIndex X0 n = X0	-- TODO: fix bounds issues
	toIndex X0 = 0
	seeIn2D = snd
	
instance Ix X0 where
	range (X0,X0) = []
	inRange (X0,X0) X0 = False


instance Show X0 where
	show X0 = "-"

type family ADD a b
type instance ADD N1 N1 = APP0 N1
type instance ADD N1 X0 = N1
type instance ADD N1 (X0_ b) = APP1 (ADD N1 b)
type instance ADD N1 (X1_ b) = APP0 b
type instance ADD X0 N1 = N1					-- MIR
type instance ADD X0 X0 = X0
type instance ADD X0 (X0_ b) = X0_ b
type instance ADD X0 (X1_ b) = APP1 b
type instance ADD (X0_ a) N1 = APP1 (ADD a N1)			-- MIR
type instance ADD (X0_ a) X0 = APP0 a				-- MIR
type instance ADD (X0_ a) (X0_ b) = APP0 (ADD a b)
type instance ADD (X0_ a) (X1_ b) = APP1 (ADD a b)
type instance ADD (X1_ a) N1 = APP0 a				-- MIR
type instance ADD (X1_ a) X0 = APP1 a				-- MIR
type instance ADD (X1_ a) (X0_ b) = APP1 (ADD a b)		-- MIR
type instance ADD (X1_ a) (X1_ b) = APP0 (SUCC (ADD a b))

type family NOT a
type instance NOT N1 = X0
type instance NOT X0 = N1
type instance NOT (X0_ a) = APP1 (NOT a)  
type instance NOT (X1_ a) = APP0 (NOT a)


type SUB a b = ADD a (SUCC (NOT b))

type D0 = X0
type D1 = X1_ X0
type D2 = X0_ (X1_ X0) 
type D3 = X1_ (X1_ X0)
type D4 = X0_ (X0_ (X1_ X0))
type D5 = X1_ (X0_ (X1_ X0))
type D6 = X0_ (X1_ (X1_ X0))
type D7 = X1_ (X1_ (X1_ X0))


type family SUCC a
type instance SUCC N1 = X0
type instance SUCC X0 = X1_ X0
type instance SUCC (X0_ a) = APP1 a
type instance SUCC (X1_ a) = APP0 (SUCC a)

type family APP1 a
type instance APP1 N1 = N1
type instance APP1 X0 = X1_ X0
type instance APP1 (X0_ a) = X1_ (X0_ a)
type instance APP1 (X1_ a) = X1_ (X1_ a)

type family APP0 a
type instance APP0 N1 = X0_ N1
type instance APP0 X0 = X0
type instance APP0 (X0_ a) = X0_ (X0_ a)
type instance APP0 (X1_ a) = X0_ (X1_ a)

	
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
	