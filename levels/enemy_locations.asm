INCLUDE "macros.asm"
INCLUDE "enemies.asm"

SECTION "level 1_1 enemies", ROMX[$6002], BANK[2]

level_1_1_enemies::
	enemy $0C, $0, $F, CHIBIBO
	enemy $0F, $0, $F, CHIBIBO | $80
	enemy $13, $0, $C, NOKOBON | $80
	enemy $21, $0, $C, CHIBIBO
	enemy $25, $0, $C, NOKOBON | $80
	enemy $28, $0, $4, CHIBIBO
	enemy $29, $0, $4, CHIBIBO
	enemy $2A, $0, $6, NOKOBON | $80
	enemy $2D, $8, $B, FLY | $80
	enemy $32, $0, $F, NOKOBON | $80
	enemy $3B, $0, $F, NOKOBON
	enemy $3C, $0, $F, NOKOBON | $80
	enemy $3D, $0, $F, NOKOBON | $80
	enemy $40, $0, $3, FLY | $80
	enemy $42, $0, $F, FLY | $80
	enemy $46, $8, $D, NOKOBON | $80
	enemy $4F, $0, $F, CHIBIBO
	enemy $52, $0, $4, FLY | $80
	enemy $52, $8, $F, FLY
	enemy $53, $8, $D, NOKOBON | $80
	enemy $55, $0, $F, CHIBIBO
	enemy $57, $0, $C, NOKOBON | $80
	enemy $5D, $0, $C, CHIBIBO
	enemy $5E, $8, $C, CHIBIBO | $80
	enemy $60, $0, $C, CHIBIBO | $80
	enemy $62, $0, $C, CHIBIBO
	enemy $64, $0, $F, CHIBIBO
	enemy $69, $0, $F, CHIBIBO
	enemy $6D, $0, $F, NOKOBON | $80
	enemy $75, $0, $F, FLY
	enemy $78, $0, $F, NOKOBON | $80
	enemy $7A, $0, $F, FLY
	enemy $80, $0, $C, NOKOBON | $80
	enemy $81, $0, $8, GAO | $80
	enemy $89, $0, $3, FLY | $80
	enemy $8E, $8, $7, HORIZONTAL_PLATFORM
	enemy $92, $8, $4, VERTICAL_PLATFORM
	enemies_end
	enemies_end

SECTION "level 1_2 enemies", ROMX[$6073], BANK[2]

level_1_2_enemies::
	enemy $0E, $0, $C, NOKOBON | $80
	enemy $10, $0, $A, NOKOBON | $80
	enemy $11, $0, $5, BUNBUN | $80
	enemy $13, $0, $8, CHIBIBO
	enemy $15, $0, $6, BUNBUN | $80
	enemy $17, $0, $8, BUNBUN
	enemy $18, $0, $8, NOKOBON | $80
	enemy $1A, $0, $7, BUNBUN | $80
	enemy $1C, $0, $5, BUNBUN
	enemy $1E, $0, $8, BUNBUN | $80
	enemy $22, $0, $C, NOKOBON
	enemy $24, $0, $A, NOKOBON
	enemy $27, $0, $8, NOKOBON | $80
	enemy $2F, $0, $4, NOKOBON
	enemy $2F, $0, $8, NOKOBON | $80
	enemy $30, $0, $8, CHIBIBO
	enemy $34, $8, $5, HORIZONTAL_PLATFORM
	enemy $37, $8, $A, HORIZONTAL_PLATFORM
	enemy $39, $0, $4, BUNBUN | $80
	enemy $3E, $0, $8, NOKOBON | $80
	enemy $3F, $0, $8, BUNBUN | $80
	enemy $42, $0, $7, BUNBUN | $80
	enemy $44, $0, $6, BUNBUN
	enemy $47, $0, $6, BUNBUN
	enemy $49, $0, $4, BUNBUN | $80
	enemy $4B, $0, $8, NOKOBON | $80
	enemy $4C, $0, $7, BUNBUN | $80
	enemy $4E, $0, $5, BUNBUN
	enemy $50, $0, $8, BUNBUN
	enemy $53, $0, $F, NOKOBON | $80
	enemy $58, $0, $C, CHIBIBO
	enemy $59, $8, $9, CHIBIBO
	enemy $5D, $8, $8, HORIZONTAL_PLATFORM
	enemy $60, $0, $4, HORIZONTAL_PLATFORM
	enemy $62, $8, $9, HORIZONTAL_PLATFORM
	enemy $67, $0, $4, BUNBUN | $80
	enemy $68, $8, $B, NOKOBON
	enemy $6B, $0, $4, BUNBUN
	enemy $6C, $0, $8, NOKOBON | $80
	enemy $74, $0, $A, VERTICAL_PLATFORM
	enemy $7E, $0, $C, NOKOBON | $80
	enemy $7F, $0, $A, BUNBUN
	enemy $81, $0, $6, BUNBUN
	enemy $84, $0, $7, HORIZONTAL_PLATFORM
	enemy $87, $8, $5, DROP_BLOCK
	enemy $88, $0, $5, DROP_BLOCK
	enemies_end

SECTION "level 1_3 enemies", ROMX[$60FE], BANK[2]

level_1_3_enemies::
	enemy $0D, $4, $D, PAKKUN_FLOWER
	enemy $0F, $0, $A, NOKOBON | $80
	enemy $13, $0, $F, NOKOBON | $80
	enemy $1B, $0, $6, FALLING_SLAB
	enemy $1C, $0, $6, FALLING_SLAB
	enemy $1D, $4, $F, PAKKUN_FLOWER
	enemy $1F, $0, $D, CHIBIBO | $80
	enemy $24, $4, $F, PAKKUN_FLOWER
	enemy $27, $4, $F, PAKKUN_FLOWER | $80
	enemy $28, $0, $6, FALLING_SLAB | $80
	enemy $28, $4, $D, PAKKUN_FLOWER
	enemy $2A, $0, $F, NOKOBON | $80
	enemy $2F, $0, $8, FALLING_SLAB | $80
	enemy $30, $0, $8, FALLING_SLAB
	enemy $31, $0, $8, FALLING_SLAB | $80
	enemy $32, $C, $E, PAKKUN_FLOWER
	enemy $35, $0, $B, NOKOBON | $80
	enemy $37, $4, $C, PAKKUN_FLOWER
	enemy $39, $C, $D, PAKKUN_FLOWER
	enemy $40, $0, $4, FALLING_SLAB
	enemy $41, $0, $4, FALLING_SLAB
	enemy $42, $8, $D, GAO | $80
	enemy $4A, $0, $B, GAO | $80
	enemy $4C, $8, $D, GAO
	enemy $51, $0, $4, FALLING_SLAB | $80
	enemy $52, $0, $4, FALLING_SLAB | $80
	enemy $54, $4, $4, PAKKUN_FLOWER | $80
	enemy $55, $8, $5, FALLING_SLAB | $80
	enemy $5E, $0, $C, NOKOBON
	enemy $5E, $0, $E, CHIBIBO | $80
	enemy $61, $8, $4, FALLING_SLAB | $80
	enemy $62, $8, $4, FALLING_SLAB | $80
	enemy $65, $8, $4, FALLING_SLAB
	enemy $66, $8, $4, FALLING_SLAB
	enemy $67, $8, $4, FALLING_SLAB
	enemy $69, $1, $0, NOKOBON | $80
	enemy $6F, $0, $A, GAO | $80
	enemy $75, $8, $D, DROP_BLOCK
	enemy $76, $0, $D, DROP_BLOCK
	enemy $76, $8, $D, DROP_BLOCK
	enemy $77, $0, $D, DROP_BLOCK
	enemy $77, $8, $D, DROP_BLOCK
	enemy $7D, $8, $E, GAO
	enemy $80, $0, $D, GAO | $80
	enemy $87, $8, $E, GAO | $80
	enemy $8A, $0, $D, GAO
	enemy $8D, $0, $5, FALLING_SLAB | $80
	enemy $92, $0, $D, KING_TOTOMESU
	enemies_end
	enemies_end

SECTION "level 2_1 enemies", ROMX[$5179], BANK[1]

level_2_1_enemies::
	enemy $0E, $1, $3, HONEN
	enemy $10, $1, $3, HONEN
	enemy $11, $0, $D, NOKOBON | $80
	enemy $12, $0, $4, NOKOBON | $80
	enemy $17, $0, $B, NOKOBON | $80
	enemy $1A, $9, $3, HONEN
	enemy $1B, $0, $5, NOKOBON | $80
	enemy $1C, $9, $3, HONEN
	enemy $21, $0, $9, VERTICAL_PLATFORM
	enemy $25, $0, $6, VERTICAL_PLATFORM
	enemy $2A, $0, $F, NOKOBON | $80
	enemy $2D, $0, $C, NOKOBON | $80
	enemy $2E, $1, $3, CHIBIBO
	enemy $2F, $0, $5, NOKOBON | $80
	enemy $34, $1, $3, HONEN
	enemy $37, $1, $3, HONEN
	enemy $3A, $1, $3, HONEN
	enemy $3D, $1, $3, HONEN
	enemy $40, $1, $3, HONEN
	enemy $41, $0, $8, NOKOBON
	enemy $43, $1, $3, HONEN
	enemy $47, $9, $3, HONEN
	enemy $49, $9, $3, HONEN
	enemy $4C, $1, $3, YURARIN_BOO | $80
	enemy $4E, $1, $3, HONEN
	enemy $51, $0, $7, CHIBIBO
	enemy $52, $0, $7, CHIBIBO
	enemy $57, $0, $4, CHIBIBO
	enemy $58, $0, $4, CHIBIBO
	enemy $59, $0, $4, CHIBIBO
	enemy $5C, $9, $3, HONEN
	enemy $5E, $9, $3, HONEN
	enemy $60, $9, $3, HONEN
	enemy $62, $9, $3, HONEN
	enemy $66, $9, $3, HONEN
	enemy $68, $9, $3, HONEN
	enemy $6A, $9, $3, YURARIN_BOO
	enemy $6C, $9, $3, HONEN | $80
	enemy $6F, $4, $E, PAKKUN_FLOWER
	enemy $71, $0, $F, NOKOBON | $80
	enemy $78, $0, $7, CHIBIBO
	enemy $79, $0, $7, CHIBIBO
	enemy $7D, $0, $B, NOKOBON | $80
	enemy $7D, $8, $7, NOKOBON | $80
	enemy $7F, $0, $4, CHIBIBO
	enemy $80, $0, $4, CHIBIBO | $80
	enemy $84, $1, $3, HONEN | $80
	enemy $87, $1, $3, YURARIN_BOO
	enemy $88, $0, $8, NOKOBON | $80
	enemy $8B, $9, $3, YURARIN_BOO
	enemy $8E, $0, $F, NOKOBON | $80
	enemy $90, $0, $8, HORIZONTAL_PLATFORM
	enemy $98, $0, $8, HORIZONTAL_PLATFORM
	enemy $99, $1, $0, NOKOBON | $80
	enemy $9C, $0, $5, DROP_BLOCK
	enemy $9C, $8, $5, DROP_BLOCK
	enemies_end

SECTION "level 2_2 enemies", ROMX[$5222], BANK[1]

level_2_2_enemies::
	enemy $0C, $0, $C, MEKABON
	enemy $12, $0, $C, NOKOBON | $80
	enemy $16, $0, $B, CHIBIBO
	enemy $17, $0, $7, NOKOBON
	enemy $1D, $0, $B, NOKOBON
	enemy $22, $0, $7, VERTICAL_PLATFORM
	enemy $23, $1, $3, YURARIN_BOO | $80
	enemy $27, $0, $7, VERTICAL_PLATFORM
	enemy $2A, $0, $D, NOKOBON
	enemy $31, $0, $9, MEKABON
	enemy $36, $0, $9, NOKOBON
	enemy $37, $0, $E, CHIBIBO
	enemy $3A, $0, $9, CHIBIBO | $80
	enemy $3E, $0, $9, MEKABON
	enemy $41, $0, $E, CHIBIBO
	enemy $44, $0, $9, CHIBIBO | $80
	enemy $46, $0, $9, NOKOBON
	enemy $48, $0, $9, MEKABON
	enemy $4B, $0, $E, CHIBIBO
	enemy $57, $8, $F, NOKOBON
	enemy $58, $0, $E, NOKOBON | $80
	enemy $59, $0, $C, CHIBIBO
	enemy $5B, $1, $3, YURARIN_BOO
	enemy $60, $8, $F, MEKABON
	enemy $65, $8, $5, HORIZONTAL_PLATFORM
	enemy $6B, $0, $A, VERTICAL_PLATFORM
	enemy $70, $0, $D, NOKOBON
	enemy $71, $1, $3, YURARIN_BOO | $80
	enemy $73, $1, $3, YURARIN_BOO
	enemy $77, $4, $B, PAKKUN_FLOWER
	enemy $78, $0, $9, NOKOBON | $80
	enemy $79, $8, $B, CHIBIBO
	enemy $7A, $C, $9, PAKKUN_FLOWER
	enemy $7D, $0, $5, NOKOBON
	enemy $7F, $0, $5, NOKOBON | $80
	enemy $83, $1, $3, YURARIN_BOO | $80
	enemy $87, $8, $5, DROP_BLOCK
	enemy $88, $0, $3, VERTICAL_PLATFORM
	enemy $88, $8, $9, DROP_BLOCK
	enemy $89, $8, $D, DROP_BLOCK
	enemies_end

SECTION "level 2_3 enemies", ROMX[$529B], BANK[1]

level_2_3_enemies::
	enemy $0F, $0, $5, ENEMY_2F | $80
	enemy $19, $0, $E, ENEMY_2F
	enemy $1B, $5, $3, HONEN
	enemy $23, $0, $E, YURARIN | $80
	enemy $25, $0, $B, YURARIN
	enemy $27, $0, $8, YURARIN | $80
	enemy $29, $0, $5, YURARIN
	enemy $2D, $0, $8, ENEMY_2F
	enemy $2F, $5, $3, HONEN
	enemy $39, $5, $3, HONEN
	enemy $3B, $0, $5, YURARIN
	enemy $3E, $0, $5, YURARIN | $80
	enemy $40, $0, $D, YURARIN
	enemy $43, $0, $D, YURARIN | $80
	enemy $43, $1, $3, HONEN
	enemy $49, $0, $7, YURARIN
	enemy $4D, $1, $3, HONEN
	enemy $4E, $0, $7, ENEMY_2F
	enemy $54, $0, $8, GUNION
	enemy $57, $0, $8, YURARIN
	enemy $5F, $0, $9, GUNION
	enemy $69, $0, $7, GUNION
	enemy $69, $0, $D, GUNION
	enemy $73, $0, $7, ENEMY_2F
	enemy $75, $1, $3, YURARIN_BOO
	enemy $78, $0, $C, YURARIN
	enemy $7F, $1, $3, YURARIN_BOO
	enemy $85, $0, $A, GUNION
	enemy $88, $0, $C, ENEMY_2F
	enemy $89, $1, $3, YURARIN_BOO | $80
	enemy $8E, $0, $F, YURARIN
	enemy $92, $0, $F, YURARIN | $80
	enemy $9B, $0, $D, GUNION
	enemy $9C, $0, $F, YURARIN | $80
	enemy $A5, $0, $7, GUNION | $80
	enemy $A8, $0, $F, YURARIN
	enemy $AE, $0, $B, TAMAO
	enemy $AF, $0, $A, TAMAO | $80
	enemy $B0, $0, $C, DRAGONZAMASU
	enemies_end

SECTION "level 3_1 enemies", ROMX[$4E74], BANK[3]

level_3_1_enemies::
	enemy $0F, $0, $F, BATADON
	enemy $10, $4, $F, PIPE_CANNON | $80
	enemy $14, $0, $F, NOKOBON
	enemy $18, $0, $F, DROP_BLOCK
	enemy $18, $8, $F, DROP_BLOCK
	enemy $19, $0, $F, DROP_BLOCK
	enemy $19, $8, $F, DROP_BLOCK
	enemy $1A, $0, $F, DROP_BLOCK
	enemy $1A, $8, $F, DROP_BLOCK
	enemy $1D, $0, $F, NOKOBON
	enemy $1F, $0, $7, BATADON | $80
	enemy $1F, $4, $E, PIPE_CANNON | $80
	enemy $23, $0, $F, NOKOBON | $80
	enemy $24, $4, $F, PIPE_CANNON
	enemy $31, $0, $4, NOKOBON
	enemy $31, $0, $4, BATADON
	enemy $34, $8, $7, HORIZONTAL_PLATFORM
	enemy $37, $8, $A, SMALL_VERTICAL_PLATFORM
	enemy $3A, $8, $9, HORIZONTAL_PLATFORM
	enemy $3C, $4, $F, PAKKUN_FLOWER
	enemy $3E, $8, $B, NOKOBON
	enemy $41, $0, $9, NOKOBON | $80
	enemy $47, $4, $E, PAKKUN_FLOWER
	enemy $4C, $4, $F, PIPE_CANNON
	enemy $51, $0, $F, TOKOTOKO | $80
	enemy $53, $0, $F, NOKOBON
	enemy $57, $0, $E, TOKOTOKO
	enemy $58, $0, $F, NOKOBON
	enemy $5E, $8, $A, NOKOBON | $80
	enemy $60, $0, $8, TOKOTOKO
	enemy $63, $0, $4, BATADON
	enemy $6A, $0, $8, NOKOBON
	enemy $6D, $0, $4, BATADON
	enemy $71, $1, $0, VERTICAL_PLATFORM
	enemy $72, $0, $6, SMALL_VERTICAL_PLATFORM
	enemy $77, $0, $6, VERTICAL_PLATFORM
	enemy $79, $4, $E, PIPE_CANNON
	enemy $7E, $4, $F, PIPE_CANNON
	enemy $86, $4, $A, PAKKUN_FLOWER
	enemy $88, $0, $F, TOKOTOKO
	enemy $89, $0, $F, TOKOTOKO
	enemy $89, $8, $7, BATADON
	enemy $8A, $0, $F, TOKOTOKO
	enemy $8D, $8, $7, NOKOBON | $80
	enemy $8E, $0, $F, BATADON
	enemy $90, $4, $A, PIPE_CANNON
	enemy $95, $0, $B, TOKOTOKO
	enemy $9B, $0, $5, GANCHAN_SPAWN
	enemy $A5, $0, $5, GANCHAN_SPAWN
	enemy $AF, $0, $5, GANCHAN_SPAWN
	enemy $B9, $0, $5, GANCHAN_SPAWN
	enemy $C3, $4, $5, GANCHAN_SPAWN
	enemy $D7, $0, $5, GANCHAN_SPAWN
	enemy $E1, $8, $A, SMALL_HORIZONTAL_PLATFORM
	enemy $E3, $0, $7, DROP_BLOCK
	enemy $E4, $0, $5, DROP_BLOCK
	enemies_end

SECTION "level 3_2 enemies", ROMX[$4F1D], BANK[3]

level_3_2_enemies::
	enemy $0C, $8, $5, SUU
	enemy $0F, $1, $0, NOKOBON
	enemy $10, $8, $C, NOKOBON | $80
	enemy $13, $0, $5, SUU
	enemy $13, $0, $F, FLY | $80
	enemy $16, $0, $F, NOKOBON
	enemy $19, $8, $8, SUU
	enemy $1A, $8, $8, SUU
	enemy $1D, $4, $E, PAKKUN_FLOWER
	enemy $1E, $0, $5, SUU
	enemy $20, $0, $4, FALLING_SPIKE | $80
	enemy $21, $C, $4, PAKKUN_FLOWER
	enemy $28, $0, $7, SUU
	enemy $2A, $0, $C, FLY
	enemy $2C, $0, $F, NOKOBON
	enemy $2D, $8, $3, FALLING_SPIKE | $80
	enemy $2E, $0, $6, SUU
	enemy $31, $0, $C, NOKOBON | $80
	enemy $32, $0, $4, FALLING_SPIKE | $80
	enemy $33, $0, $2, NOKOBON
	enemy $34, $0, $C, NOKOBON | $80
	enemy $37, $8, $F, NOKOBON
	enemy $3B, $8, $6, FALLING_SPIKE
	enemy $3E, $0, $C, NOKOBON | $80
	enemy $40, $0, $C, NOKOBON
	enemy $44, $0, $6, SUU
	enemy $45, $0, $B, FLY
	enemy $46, $0, $8, SUU
	enemy $49, $0, $F, NOKOBON
	enemy $4B, $8, $4, FALLING_SPIKE | $80
	enemy $4E, $0, $8, FLY
	enemy $4F, $0, $C, NOKOBON | $80
	enemy $51, $C, $E, PIPE_CANNON
	enemy $53, $0, $5, SUU
	enemy $55, $0, $F, FLY | $80
	enemy $56, $0, $C, NOKOBON
	enemy $58, $0, $E, FLY | $80
	enemy $63, $0, $B, FLY
	enemy $69, $8, $3, GANCHAN_SPAWN
	enemy $73, $8, $3, GANCHAN_SPAWN
	enemy $7B, $0, $5, FALLING_SPIKE | $80
	enemy $7B, $C, $F, PIPE_CANNON
	enemy $7C, $8, $8, SUU
	enemy $7E, $0, $F, FLY
	enemy $7F, $0, $F, NOKOBON | $80
	enemy $81, $4, $E, PAKKUN_FLOWER
	enemy $84, $0, $8, HORIZONTAL_PLATFORM
	enemy $88, $0, $D, NOKOBON
	enemy $89, $4, $7, SMALL_VERTICAL_PLATFORM
	enemy $8C, $8, $A, DROP_BLOCK
	enemy $8D, $0, $A, DROP_BLOCK
	enemy $8E, $8, $9, DROP_BLOCK
	enemy $8F, $0, $9, DROP_BLOCK
	enemy $92, $8, $A, DROP_BLOCK
	enemy $94, $0, $B, DROP_BLOCK
	enemy $94, $8, $B, DROP_BLOCK
	enemy $96, $8, $B, DROP_BLOCK
	enemy $98, $0, $A, DROP_BLOCK
	enemy $99, $8, $9, DROP_BLOCK
	enemy $9B, $0, $8, DROP_BLOCK
	enemy $9C, $8, $7, DROP_BLOCK
	enemy $9E, $0, $5, DROP_BLOCK
	enemies_end

SECTION "level 3_3 enemies", ROMX[$4FD8], BANK[3]

level_3_3_enemies::
	enemy $0E, $8, $C, VERTICAL_PLATFORM
	enemy $12, $0, $9, DIAGONAL_PLATFORM_NE
	enemy $14, $0, $9, DIAGONAL_PLATFORM_NW
	enemy $19, $8, $8, DIAGONAL_PLATFORM_NE
	enemy $1D, $0, $5, SMALL_HORIZONTAL_PLATFORM
	enemy $26, $0, $A, SMALL_VERTICAL_PLATFORM
	enemy $27, $0, $8, DROP_BLOCK
	enemy $27, $8, $8, DROP_BLOCK
	enemy $27, $0, $D, GANCHAN
	enemy $28, $C, $6, PAKKUN_FLOWER
	enemy $29, $0, $D, NOKOBON
	enemy $35, $1, $0, FLY
	enemy $37, $0, $7, NOKOBON
	enemy $39, $1, $0, FLY
	enemy $3F, $0, $E, VERTICAL_PLATFORM
	enemy $41, $8, $C, DIAGONAL_PLATFORM_NE
	enemy $44, $8, $C, DIAGONAL_PLATFORM_NW
	enemy $46, $8, $4, SMALL_VERTICAL_PLATFORM
	enemy $4C, $0, $4, VERTICAL_PLATFORM
	enemy $4E, $1, $0, VERTICAL_PLATFORM
	enemy $55, $0, $9, DROP_BLOCK
	enemy $55, $8, $9, DROP_BLOCK
	enemy $57, $1, $0, VERTICAL_PLATFORM
	enemy $5D, $1, $0, SMALL_HORIZONTAL_PLATFORM
	enemy $60, $9, $1, SMALL_HORIZONTAL_PLATFORM
	enemy $6F, $0, $F, BATADON
	enemy $71, $0, $F, TOKOTOKO | $80
	enemy $73, $0, $F, TOKOTOKO
	enemy $7A, $8, $F, BATADON | $80
	enemy $7B, $0, $A, NOKOBON | $80
	enemy $7C, $8, $F, GANCHAN
	enemy $88, $0, $C, BATADON | $80
	enemy $89, $8, $A, BATADON
	enemy $92, $0, $E, HIYOIHOI
	enemies_end

SECTION "level 4_1 enemies", ROMX[$5311], BANK[1]

level_4_1_enemies::
	enemy $0F, $C, $C, REVERSE_PAKKUN_FLOWER
	enemy $11, $D, $1, PAKKUN_FLOWER
	enemy $19, $5, $1, PAKKUN_FLOWER
	enemy $1A, $0, $B, PIONPI | $80
	enemy $1B, $0, $F, NOKOBON | $80
	enemy $1C, $0, $F, PIONPI
	enemy $1F, $8, $9, HORIZONTAL_PLATFORM
	enemy $24, $8, $7, VERTICAL_PLATFORM
	enemy $2C, $0, $F, NOKOBON | $80
	enemy $2D, $5, $1, PIPE_CANNON
	enemy $2F, $0, $F, PIONPI
	enemy $37, $0, $E, NOKOBON | $80
	enemy $39, $8, $E, NOKOBON
	enemy $3A, $0, $E, PIONPI
	enemy $40, $0, $E, PIONPI
	enemy $41, $0, $E, NOKOBON
	enemy $43, $8, $E, NOKOBON
	enemy $4A, $0, $F, PIONPI
	enemy $4B, $5, $1, PAKKUN_FLOWER
	enemy $4D, $0, $B, PIONPI
	enemy $4D, $0, $F, NOKOBON | $80
	enemy $51, $5, $1, PAKKUN_FLOWER | $80
	enemy $53, $5, $1, PAKKUN_FLOWER
	enemy $54, $4, $F, PAKKUN_FLOWER
	enemy $55, $C, $D, PIPE_CANNON
	enemy $5B, $4, $E, PAKKUN_FLOWER
	enemy $5E, $0, $A, PIONPI
	enemy $5E, $8, $A, NOKOBON
	enemy $62, $4, $D, PAKKUN_FLOWER
	enemy $63, $4, $E, PIPE_CANNON | $80
	enemy $67, $0, $E, DIAGONAL_PLATFORM_NW
	enemy $6B, $0, $A, DIAGONAL_PLATFORM_NE
	enemy $6F, $C, $F, PAKKUN_FLOWER
	enemy $71, $C, $C, REVERSE_PAKKUN_FLOWER
	enemy $73, $C, $C, REVERSE_PAKKUN_FLOWER
	enemy $75, $D, $1, PAKKUN_FLOWER
	enemy $79, $C, $F, PIPE_CANNON
	enemy $7B, $C, $C, REVERSE_PAKKUN_FLOWER
	enemy $7C, $0, $F, CHIBIBO | $80
	enemy $7D, $C, $C, REVERSE_PAKKUN_FLOWER
	enemy $7E, $0, $F, CHIBIBO
	enemy $7F, $D, $1, PAKKUN_FLOWER
	enemy $83, $C, $F, PIPE_CANNON
	enemy $85, $C, $C, REVERSE_PAKKUN_FLOWER
	enemy $87, $C, $C, REVERSE_PAKKUN_FLOWER
	enemy $88, $0, $F, NOKOBON | $80
	enemy $89, $D, $1, PAKKUN_FLOWER
	enemy $8A, $8, $6, FALLING_SLAB
	enemy $8E, $8, $D, VERTICAL_PLATFORM
	enemy $92, $0, $6, DIAGONAL_PLATFORM_NE
	enemy $95, $8, $5, DROP_BLOCK
	enemy $96, $0, $5, DROP_BLOCK
	enemy $98, $0, $5, DROP_BLOCK
	enemy $98, $8, $5, DROP_BLOCK
	enemy $9A, $0, $6, DROP_BLOCK
	enemy $9A, $8, $6, DROP_BLOCK
	enemy $9D, $0, $6, HORIZONTAL_PLATFORM
	enemy $A1, $5, $1, PAKKUN_FLOWER | $80
	enemy $A2, $5, $1, PIPE_CANNON
	enemy $A3, $5, $1, PAKKUN_FLOWER | $80
	enemy $A4, $4, $F, PAKKUN_FLOWER | $80
	enemy $A5, $C, $D, PIPE_CANNON | $80
	enemy $A7, $8, $9, PIONPI
	enemy $AE, $0, $E, PIONPI
	enemy $AF, $0, $E, NOKOBON
	enemy $B1, $8, $E, NOKOBON
	enemy $B2, $0, $E, PIONPI
	enemy $B4, $0, $9, HORIZONTAL_PLATFORM
	enemy $C2, $D, $1, PIPE_CANNON
	enemy $C4, $C, $F, PIPE_CANNON
	enemy $C5, $8, $A, NOKOBON
	enemy $C6, $C, $9, PIPE_CANNON
	enemy $CF, $0, $5, PIONPI
	enemy $CF, $0, $8, PIONPI
	enemy $CF, $0, $E, PIONPI
	enemy $D4, $9, $0, DROP_BLOCK
	enemy $D5, $1, $0, DROP_BLOCK
	enemy $D7, $D, $1, PIPE_CANNON
	enemy $D8, $C, $F, PIPE_CANNON
	enemy $D9, $C, $C, PIPE_CANNON
	enemy $DE, $8, $8, DIAGONAL_PLATFORM_NW
	enemies_end

SECTION "level 4_2 enemies", ROMX[$5405], BANK[1]

level_4_2_enemies::
	enemy $0F, $0, $B, NOKOBON
	enemy $11, $0, $9, POMPON_FLOWER | $80
	enemy $15, $0, $F, NOKOBON
	enemy $19, $0, $B, NOKOBON
	enemy $1B, $0, $F, POMPON_FLOWER
	enemy $1F, $5, $0, PIPE_CANNON
	enemy $20, $0, $F, NOKOBON | $80
	enemy $23, $0, $D, NOKOBON
	enemy $27, $0, $B, POMPON_FLOWER
	enemy $29, $5, $0, PIPE_CANNON
	enemy $2A, $0, $F, NOKOBON | $80
	enemy $2D, $8, $D, GAO | $80
	enemy $30, $0, $B, GAO
	enemy $34, $C, $D, REVERSE_PAKKUN_FLOWER
	enemy $37, $0, $9, NOKOBON
	enemy $37, $C, $D, REVERSE_PAKKUN_FLOWER
	enemy $3A, $C, $C, REVERSE_PAKKUN_FLOWER
	enemy $3E, $C, $D, REVERSE_PAKKUN_FLOWER
	enemy $40, $5, $0, PIPE_CANNON | $80
	enemy $41, $C, $D, REVERSE_PAKKUN_FLOWER
	enemy $43, $5, $0, PIPE_CANNON
	enemy $44, $C, $C, REVERSE_PAKKUN_FLOWER
	enemy $47, $0, $9, NOKOBON
	enemy $48, $0, $F, CHIBIBO
	enemy $4B, $0, $C, GAO | $80
	enemy $4D, $0, $E, POMPON_FLOWER
	enemy $53, $0, $F, ROTO_DISC
	enemy $54, $4, $C, PAKKUN_FLOWER
	enemy $56, $0, $7, ROTO_DISC
	enemy $59, $0, $F, ROTO_DISC
	enemy $5A, $0, $F, CHIBIBO
	enemy $5D, $0, $F, ROTO_DISC
	enemy $60, $0, $7, ROTO_DISC
	enemy $63, $0, $F, ROTO_DISC
	enemy $68, $0, $C, ROTO_DISC
	enemy $6A, $0, $F, NOKOBON
	enemy $6C, $0, $D, ROTO_DISC
	enemy $70, $8, $F, ROTO_DISC | $80
	enemy $72, $8, $E, VERTICAL_PLATFORM
	enemy $76, $8, $C, VERTICAL_PLATFORM
	enemy $7A, $0, $F, NOKOBON
	enemy $7C, $0, $C, ROTO_DISC
	enemy $7E, $0, $F, NOKOBON
	enemy $80, $0, $D, ROTO_DISC
	enemy $84, $8, $F, ROTO_DISC | $80
	enemy $86, $8, $E, VERTICAL_PLATFORM
	enemy $8A, $8, $C, VERTICAL_PLATFORM
	enemy $8B, $8, $7, ROTO_DISC | $80
	enemy $8F, $0, $F, NOKOBON
	enemy $91, $0, $C, GAO
	enemy $94, $8, $A, NOKOBON
	enemy $95, $8, $7, ROTO_DISC
	enemy $99, $0, $F, ROTO_DISC
	enemy $9C, $0, $7, ROTO_DISC
	enemy $9D, $8, $D, NOKOBON
	enemy $9F, $0, $F, ROTO_DISC
	enemy $A3, $0, $F, GAO
	enemy $A5, $0, $C, NOKOBON
	enemy $A7, $8, $D, GAO | $80
	enemy $A8, $8, $A, NOKOBON
	enemy $A9, $8, $7, ROTO_DISC
	enemy $B1, $0, $F, GIRA | $80
	enemy $B5, $4, $C, REVERSE_PAKKUN_FLOWER
	enemy $B7, $0, $6, SMALL_VERTICAL_PLATFORM
	enemy $BC, $0, $6, DROP_BLOCK
	enemy $BF, $0, $E, DROP_BLOCK
	enemy $BF, $8, $B, DROP_BLOCK
	enemy $C0, $0, $8, DROP_BLOCK
	enemy $C0, $8, $6, DROP_BLOCK
	enemies_end

SECTION "level 4_3 enemies", ROMX[$54D5], BANK[1]

level_4_3_enemies::
	enemy $10, $0, $6, CHICKEN
	enemy $11, $0, $F, CHICKEN | $80
	enemy $13, $0, $8, CHICKEN
	enemy $14, $0, $D, CHICKEN
	enemy $17, $0, $A, CHICKEN
	enemy $19, $0, $6, CHICKEN
	enemy $1A, $0, $F, CHICKEN | $80
	enemy $1C, $0, $C, CHICKEN
	enemy $1D, $0, $9, CHICKEN
	enemy $23, $0, $6, CHICKEN
	enemy $24, $0, $8, CHICKEN | $80
	enemy $25, $0, $A, CHICKEN
	enemy $27, $0, $E, CHICKEN
	enemy $28, $0, $C, CHICKEN | $80
	enemy $29, $0, $A, CHICKEN
	enemy $2B, $0, $6, CHICKEN
	enemy $2C, $0, $5, ROKETON
	enemy $2E, $0, $5, ROKETON | $80
	enemy $30, $0, $5, ROKETON
	enemy $34, $0, $F, ROKETON
	enemy $36, $0, $F, ROKETON | $80
	enemy $38, $0, $F, ROKETON
	enemy $3C, $0, $5, ROKETON
	enemy $3D, $0, $A, ROKETON | $80
	enemy $3E, $0, $F, ROKETON
	enemy $42, $0, $6, ROKETON
	enemy $43, $0, $D, ROKETON
	enemy $4C, $0, $5, CHICKEN
	enemy $4D, $8, $B, CHIKAKO
	enemy $4E, $0, $6, CHICKEN | $80
	enemy $4E, $0, $C, ROKETON
	enemy $50, $8, $5, CHIKAKO
	enemy $52, $0, $F, ROKETON
	enemy $52, $0, $6, CHICKEN
	enemy $56, $0, $6, CHIKAKO
	enemy $57, $0, $E, CHICKEN
	enemy $58, $8, $F, CHIKAKO
	enemy $5A, $0, $6, ROKETON
	enemy $5A, $0, $E, ROKETON | $80
	enemy $5C, $0, $F, CHICKEN
	enemy $5D, $0, $9, CHIKAKO
	enemy $5F, $0, $8, CHICKEN
	enemy $60, $0, $D, CHIKAKO
	enemy $63, $0, $5, CHICKEN
	enemy $63, $0, $A, ROKETON
	enemy $65, $0, $E, CHICKEN
	enemy $67, $0, $9, CHIKAKO
	enemy $68, $0, $9, CHICKEN
	enemy $69, $0, $E, ROKETON | $80
	enemy $6B, $0, $9, CHIKAKO
	enemy $6C, $0, $8, CHICKEN
	enemy $71, $8, $A, CHIKAKO
	enemy $72, $0, $7, CHICKEN
	enemy $73, $0, $A, ROKETON
	enemy $75, $0, $C, ROKETON
	enemy $76, $8, $F, CHIKAKO
	enemy $78, $0, $8, ROKETON
	enemy $7A, $0, $A, CHICKEN
	enemy $7B, $0, $E, CHIKAKO
	enemy $7D, $0, $7, CHICKEN
	enemy $7E, $0, $D, CHICKEN
	enemy $80, $8, $C, CHIKAKO
	enemy $85, $0, $5, CHICKEN
	enemy $87, $0, $E, CHICKEN
	enemy $89, $0, $E, ROKETON | $80
	enemy $8E, $0, $A, ROKETON
	enemy $90, $0, $7, ROKETON
	enemy $93, $0, $D, CHICKEN
	enemy $93, $0, $6, ROKETON
	enemy $CF, $8, $A, ROTO_DISC
	enemy $D9, $8, $7, ROTO_DISC | $80
	enemy $DB, $0, $C, ROTO_DISC
	enemy $DC, $0, $D, GENKOTSU | $80
	enemy $E0, $0, $8, GENKOTSU
	enemy $E1, $0, $8, GENKOTSU
	enemy $EC, $8, $A, BIOKINTON
	enemies_end
	enemies_end

