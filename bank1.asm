SECTION "bank 1", ROMX, BANK[1]
; Unused slots are filled with repeats of other pointers
; todo should be named levelscreenpointers or so?
LevelPointers:: ; 4000 Same every bank
LevelPointersBank1:: ; 1:4000
	dw $55BB
	dw $55E2
	dw $5605
	dw $55BB	; 2-1
	dw $55E2	; 2-2
	dw $5605	; 2-3
	dw $55BB
	dw $55E2
	dw $5605
	dw $5630	; 4-1
	dw $5665	; 4-2
	dw $5694	; 4-3
	dw $55BB

LevelEnemyPointers:: ; 401A
LevelEnemyPointersBank1:: ; 1:401A
	dw $5311
	dw $5405
	dw $54D5
	dw $5179	; 2-1
	dw $5222	; 2-2
	dw $529B	; 2-3
	dw $5311
	dw $5405
	dw $54D5
	dw $5311	; 4-1
	dw $5405	; 4-2
	dw $54D5	; 4-3

INCBIN "gfx/enemiesWorld2.2bpp"
INCBIN "gfx/backgroundWorld2.2bpp"

INCBIN "gfx/enemiesWorld4.2bpp"
INCBIN "gfx/backgroundWorld4.2bpp"

Call_001_4FB2::
    ldh a, [$AC]                                  ; $4FB2: $F0 $AC
    and $01                                       ; $4FB4: $E6 $01
    ret nz                                        ; $4FB6: $C0

    ld a, [$C0D2]                                 ; $4FB7: $FA $D2 $C0
    cp $07                                        ; $4FBA: $FE $07
    jr c, jr_001_4FCB                             ; $4FBC: $38 $0D

    ldh a, [$A4]                                  ; $4FBE: $F0 $A4
    and $0C                                       ; $4FC0: $E6 $0C
    jr nz, jr_001_4FCB                            ; $4FC2: $20 $07

    ldh a, [$A4]                                  ; $4FC4: $F0 $A4
    and $FC                                       ; $4FC6: $E6 $FC
    ldh [$A4], a                                  ; $4FC8: $E0 $A4
    ret                                           ; $4FCA: $C9


jr_001_4FCB:
    ldh a, [$A4]                                  ; $4FCB: $F0 $A4
    inc a                                         ; $4FCD: $3C
    ldh [$A4], a                                  ; $4FCE: $E0 $A4
    ld b, $01                                     ; $4FD0: $06 $01
    call MoveMario.call_1EA4                      ; $4FD2: $CD $A4 $1E
    call ScrollEnemiesByB                         ; $4FD5: $CD $9F $2C
    ld hl, wMarioPosX                             ; $4FD8: $21 $02 $C2
    dec [hl]                                      ; $4FDB: $35
    ld a, [hl]                                    ; $4FDC: $7E
    and a                                         ; $4FDD: $A7
    jr nz, jr_001_4FE2                            ; $4FDE: $20 $02

    ld [hl], $F0                                  ; $4FE0: $36 $F0

jr_001_4FE2:
    ld c, $08                                     ; $4FE2: $0E $08
    call Call_001_50CC                            ; $4FE4: $CD $CC $50
    ld hl, wMarioPosX                             ; $4FE7: $21 $02 $C2
    inc [hl]                                      ; $4FEA: $34
    ret                                           ; $4FEB: $C9

Call_01_4FEC::
    ldh a, [$80]                                  ; $4FEC: $F0 $80
    bit 6, a                                      ; $4FEE: $CB $77
    jr nz, jr_001_5034                            ; $4FF0: $20 $42

    bit 7, a                                      ; $4FF2: $CB $7F
    jr nz, jr_001_5022                            ; $4FF4: $20 $2C

jr_001_4FF6:
    ldh a, [$80]                                  ; $4FF6: $F0 $80
    bit 4, a                                      ; $4FF8: $CB $67
    jr nz, jr_001_5014                            ; $4FFA: $20 $18

    bit 5, a                                      ; $4FFC: $CB $6F
    ret z                                         ; $4FFE: $C8

    ld c, $FA                                     ; $4FFF: $0E $FA
    call Call_001_50CC                            ; $5001: $CD $CC $50
    ld hl, wMarioPosX                             ; $5004: $21 $02 $C2
    ld a, [hl]                                    ; $5007: $7E
    cp $10                                        ; $5008: $FE $10
    ret c                                         ; $500A: $D8

    dec [hl]                                      ; $500B: $35
    ld a, [$C0D2]                                 ; $500C: $FA $D2 $C0
    cp $07                                        ; $500F: $FE $07
    ret nc                                        ; $5011: $D0

    dec [hl]                                      ; $5012: $35
    ret                                           ; $5013: $C9


jr_001_5014:
    ld c, $08                                     ; $5014: $0E $08
    call Call_001_50CC                            ; $5016: $CD $CC $50
    ld hl, wMarioPosX                             ; $5019: $21 $02 $C2
    ld a, [hl]                                    ; $501C: $7E
    cp $A0                                        ; $501D: $FE $A0
    ret nc                                        ; $501F: $D0

    inc [hl]                                      ; $5020: $34
    ret                                           ; $5021: $C9


jr_001_5022:
    call Call_001_5089                            ; $5022: $CD $89 $50
    cp $FF                                        ; $5025: $FE $FF
    jr z, jr_001_4FF6                             ; $5027: $28 $CD

    ld hl, wMarioPosY                             ; $5029: $21 $01 $C2
    ld a, [hl]                                    ; $502C: $7E
    cp $94                                        ; $502D: $FE $94
    jr nc, jr_001_4FF6                            ; $502F: $30 $C5

    inc [hl]                                      ; $5031: $34
    jr jr_001_4FF6                                ; $5032: $18 $C2

jr_001_5034:
    call Call_001_5046                            ; $5034: $CD $46 $50
    cp $FF                                        ; $5037: $FE $FF
    jr z, jr_001_4FF6                             ; $5039: $28 $BB

    ld hl, wMarioPosY                             ; $503B: $21 $01 $C2
    ld a, [hl]                                    ; $503E: $7E
    cp $30                                        ; $503F: $FE $30
    jr c, jr_001_4FF6                             ; $5041: $38 $B3

    dec [hl]                                      ; $5043: $35
    jr jr_001_4FF6                                ; $5044: $18 $B0

Call_001_5046:
    ld hl, wMarioPosY                             ; $5046: $21 $01 $C2
    ldh a, [$99]                                  ; $5049: $F0 $99
    ld b, $FD                                     ; $504B: $06 $FD
    and a                                         ; $504D: $A7
    jr z, jr_001_5052                             ; $504E: $28 $02

    ld b, $FC                                     ; $5050: $06 $FC

jr_001_5052:
    ld a, [hli]                                   ; $5052: $2A
    add b                                         ; $5053: $80
    ldh [$AD], a                                  ; $5054: $E0 $AD
    ldh a, [$A4]                                  ; $5056: $F0 $A4
    ld b, [hl]                                    ; $5058: $46
    add b                                         ; $5059: $80
    add $02                                       ; $505A: $C6 $02
    ldh [$AE], a                                  ; $505C: $E0 $AE
    call LookupTile                               ; $505E: $CD $53 $01
    cp $60                                        ; $5061: $FE $60
    jr nc, jr_001_5071                            ; $5063: $30 $0C

    ldh a, [$AE]                                  ; $5065: $F0 $AE
    add $FA                                       ; $5067: $C6 $FA
    ldh [$AE], a                                  ; $5069: $E0 $AE
    call LookupTile                               ; $506B: $CD $53 $01
    cp $60                                        ; $506E: $FE $60
    ret c                                         ; $5070: $D8

jr_001_5071:
    cp $F4                                        ; $5071: $FE $F4
    jr z, jr_001_5078                             ; $5073: $28 $03

    ld a, $FF                                     ; $5075: $3E $FF
    ret                                           ; $5077: $C9


jr_001_5078:
    push hl                                       ; $5078: $E5
    pop de                                        ; $5079: $D1
    ld hl, $FFEE                                  ; $507A: $21 $EE $FF
    ld [hl], $C0                                  ; $507D: $36 $C0
    inc l                                         ; $507F: $2C
    ld [hl], d                                    ; $5080: $72
    inc l                                         ; $5081: $2C
    ld [hl], e                                    ; $5082: $73
    ld a, $05                                     ; $5083: $3E $05
    ld [$DFE0], a                                 ; $5085: $EA $E0 $DF
    ret                                           ; $5088: $C9


Call_001_5089:
    ld hl, wMarioPosY                             ; $5089: $21 $01 $C2
    ld a, [hli]                                   ; $508C: $2A
    add $0A                                       ; $508D: $C6 $0A
    ldh [$AD], a                                  ; $508F: $E0 $AD
    ldh a, [$A4]                                  ; $5091: $F0 $A4
    ld b, a                                       ; $5093: $47
    ld a, [hl]                                    ; $5094: $7E
    add b                                         ; $5095: $80
    add $FE                                       ; $5096: $C6 $FE
    ldh [$AE], a                                  ; $5098: $E0 $AE
    call LookupTile                               ; $509A: $CD $53 $01
    cp $60                                        ; $509D: $FE $60
    jr nc, jr_001_50B4                            ; $509F: $30 $13

    ldh a, [$AE]                                  ; $50A1: $F0 $AE
    add $04                                       ; $50A3: $C6 $04
    ldh [$AE], a                                  ; $50A5: $E0 $AE
    call LookupTile                               ; $50A7: $CD $53 $01
    cp $E1                                        ; $50AA: $FE $E1
    jp z, Jmp_1B45                                ; $50AC: $CA $45 $1B

    cp $60                                        ; $50AF: $FE $60
    jr nc, jr_001_50B4                            ; $50B1: $30 $01

    ret                                           ; $50B3: $C9


jr_001_50B4:
    cp $F4                                        ; $50B4: $FE $F4
    jr nz, jr_001_50C9                            ; $50B6: $20 $11

    push hl                                       ; $50B8: $E5
    pop de                                        ; $50B9: $D1
    ld hl, $FFEE                                  ; $50BA: $21 $EE $FF
    ld [hl], $C0                                  ; $50BD: $36 $C0
    inc l                                         ; $50BF: $2C
    ld [hl], d                                    ; $50C0: $72
    inc l                                         ; $50C1: $2C
    ld [hl], e                                    ; $50C2: $73
    ld a, $05                                     ; $50C3: $3E $05
    ld [$DFE0], a                                 ; $50C5: $EA $E0 $DF
    ret                                           ; $50C8: $C9


jr_001_50C9:
    ld a, $FF                                     ; $50C9: $3E $FF
    ret                                           ; $50CB: $C9


Call_001_50CC:
    ld de, $0502                                  ; $50CC: $11 $02 $05
    ldh a, [$99]                                  ; $50CF: $F0 $99
    cp $02                                        ; $50D1: $FE $02
    jr z, jr_001_50D8                             ; $50D3: $28 $03

    ld de, $0501                                  ; $50D5: $11 $01 $05

jr_001_50D8:
    ld hl, wMarioPosY                             ; $50D8: $21 $01 $C2
    ld a, [hli]                                   ; $50DB: $2A
    add d                                         ; $50DC: $82
    ldh [$AD], a                                  ; $50DD: $E0 $AD
    ld b, [hl]                                    ; $50DF: $46
    ld a, c                                       ; $50E0: $79
    add b                                         ; $50E1: $80
    ld b, a                                       ; $50E2: $47
    ldh a, [$A4]                                  ; $50E3: $F0 $A4
    add b                                         ; $50E5: $80
    ldh [$AE], a                                  ; $50E6: $E0 $AE
    push de                                       ; $50E8: $D5
    call LookupTile                               ; $50E9: $CD $53 $01
    pop de                                        ; $50EC: $D1
    cp $60                                        ; $50ED: $FE $60
    jr c, jr_001_5101                             ; $50EF: $38 $10

    cp $F4                                        ; $50F1: $FE $F4
    jr z, jr_001_5107                             ; $50F3: $28 $12

    cp $E1                                        ; $50F5: $FE $E1
    jp z, Jmp_1B45                                ; $50F7: $CA $45 $1B

    cp $83                                        ; $50FA: $FE $83
    jp z, Jmp_1B45                                ; $50FC: $CA $45 $1B

    pop hl                                        ; $50FF: $E1
    ret                                           ; $5100: $C9


jr_001_5101:
    ld d, $FD                                     ; $5101: $16 $FD
    dec e                                         ; $5103: $1D
    jr nz, jr_001_50D8                            ; $5104: $20 $D2

    ret                                           ; $5106: $C9


jr_001_5107:
    push hl                                       ; $5107: $E5
    pop de                                        ; $5108: $D1
    ld hl, $FFEE                                  ; $5109: $21 $EE $FF
    ld [hl], $C0                                  ; $510C: $36 $C0
    inc l                                         ; $510E: $2C
    ld [hl], d                                    ; $510F: $72
    inc l                                         ; $5110: $2C
    ld [hl], e                                    ; $5111: $73
    ld a, $05                                     ; $5112: $3E $05
    ld [$DFE0], a                                 ; $5114: $EA $E0 $DF
    ret                                           ; $5117: $C9

Call_01_5118::
    ld b, $03                                     ; $5118: $06 $03
    ld hl, $FFA9                                  ; $511A: $21 $A9 $FF
    ld de, $C001                                  ; $511D: $11 $01 $C0

jr_001_5120:
    ld a, [hli]                                   ; $5120: $2A
    and a                                         ; $5121: $A7
    jr nz, jr_001_512C                            ; $5122: $20 $08

jr_001_5124:
    inc e                                         ; $5124: $1C
    inc e                                         ; $5125: $1C
    inc e                                         ; $5126: $1C
    inc e                                         ; $5127: $1C
    dec b                                         ; $5128: $05
    jr nz, jr_001_5120                            ; $5129: $20 $F5

    ret                                           ; $512B: $C9

jr_001_512C:
    push hl                                       ; $512C: $E5
    push de                                       ; $512D: $D5
    push bc                                       ; $512E: $C5
    dec l                                         ; $512F: $2D
    ld a, [de]                                    ; $5130: $1A
    inc a                                         ; $5131: $3C
    inc a                                         ; $5132: $3C
    ld [de], a                                    ; $5133: $12
    ldh [$A1], a                                  ; $5134: $E0 $A1
    ldh [$C3], a                                  ; $5136: $E0 $C3
    cp $A9                                        ; $5138: $FE $A9
    jr c, jr_001_5143                             ; $513A: $38 $07

jr_001_513C:
    xor a                                         ; $513C: $AF
    res 0, e                                      ; $513D: $CB $83
    ld [de], a                                    ; $513F: $12
    ld [hl], a                                    ; $5140: $77
    jr jr_001_5156                                ; $5141: $18 $13

jr_001_5143:
    add $02                                       ; $5143: $C6 $02
    push af                                       ; $5145: $F5
    dec e                                         ; $5146: $1D
    ld a, [de]                                    ; $5147: $1A
    ldh [$C2], a                                  ; $5148: $E0 $C2
    add $06                                       ; $514A: $C6 $06
    ldh [$AD], a                                  ; $514C: $E0 $AD
    pop af                                        ; $514E: $F1
    call FindNeighboringTile                      ; $514F: $CD $D2 $1F
    jr c, jr_001_5156                             ; $5152: $38 $02

    jr jr_001_513C                                ; $5154: $18 $E6

jr_001_5156:
    pop bc                                        ; $5156: $C1
    pop de                                        ; $5157: $D1
    pop hl                                        ; $5158: $E1
    call Call_200A                                ; $5159: $CD $0A $20
    jr jr_001_5124                                ; $515C: $18 $C6

Call_01_515E::
    ld a, [wMarioPosX]                            ; $515E: $FA $02 $C2
    cp $01                                        ; $5161: $FE $01
    jr c, jr_001_5168                             ; $5163: $38 $03

    cp $ED                                        ; $5165: $FE $ED
    ret c                                         ; $5167: $D8

jr_001_5168:
    xor a                                         ; $5168: $AF
    ldh [$99], a                                  ; $5169: $E0 $99
    ldh [$B5], a                                  ; $516B: $E0 $B5
    inc a                                         ; $516D: $3C
    ldh [$B3], a                                  ; $516E: $E0 $B3
    inc a                                         ; $5170: $3C
    ld [wActiveMusic], a                                 ; $5171: $EA $E8 $DF
    ld a, $90                                     ; $5174: $3E $90
    ldh [$A6], a                                  ; $5176: $E0 $A6
    ret  

SECTION "bank 1 levels", ROMX[$55BB], BANK[1]
INCBIN "baserom.gb", $55BB, $8000 - $55BB
