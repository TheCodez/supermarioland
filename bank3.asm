INCLUDE "hardware.inc"
INCLUDE "sound_constants.asm"
INCLUDE "hram.asm"

SECTION "bank 3", ROMX, BANK[3]
LevelPointersBank3:: ; 3:4000
	dw $503F
	dw $5074
	dw $509B
	dw $503F
	dw $5074
	dw $509B
	dw $503F	; 3-1
	dw $5074	; 3-2
	dw $509B	; 3-3
	dw $503F
	dw $5074
	dw $509B
	dw $50C0	; End of world "hangar"

LevelEnemyPointersBank3:: ; 3:401A
	dw $4E74
	dw $4F1D
	dw $4FD8
	dw $4E74
	dw $4F1D
	dw $4FD8
	dw $4E74	; 3-1
	dw $4F1D	; 3-2
	dw $4FD8	; 3-3
	dw $4E74
	dw $4F1D
	dw $4FD8

INCBIN "gfx/enemiesWorld3.2bpp"
INCBIN "gfx/backgroundWorld3.2bpp"

INCLUDE "joypad.asm"

Call_4823:: ; 4823
	ld a, h
	ldh [$FF96], a
	ld a, l
	ldh [$FF97], a
	ld a, [hl]
	and a
	jr z, .jmp_484A
	cp a, $80
	jr z, .jmp_4848
.jmp_4831
	ldh a, [$FF96]
	ld h, a
	ldh a, [$FF97]
	ld l, a
	ld de, $10
	add hl, de
	ldh a, [$FF8F]
	dec a
	ldh [$FF8F], a
	ret z
	jr Call_4823

.jmp_4843
	xor a
	ldh [$FF95], a
	jr .jmp_4831

.jmp_4848
	ldh [$FF95], a
.jmp_484A
	ld b, 7
	ld de, $FF86
.jmp_484F
	ld a, [hli]
	ld [de], a
	inc de
	dec b
	jr nz, .jmp_484F
	ldh a, [$FF89]
	ld hl, Data_4C37
	rlca
	ld e, a
	ld d, $00
	add hl, de
	ld e, [hl]
	inc hl
	ld d, [hl]
	ld a, [de]
	ld l, a
	inc de
	ld a, [de]
	ld h, a
	inc de
	ld a, [de]
	ldh [$FF90], a
	inc de
	ld a, [de]
	ldh [$FF91], a
	ld e, [hl]
	inc hl
	ld d, [hl]
.jmp_4872
	inc hl
	ldh a, [$FF8C]
	ldh [$FF94], a
	ld a, [hl]
	cp a, $FF
	jr z, .jmp_4843
	cp a, $FD
	jr nz, .jmp_488C
	ldh a, [$FF8C]
	xor a, $10
	ldh [$FF94], a
	jr .jmp_4872

.jmp_4888
	inc de
	inc de
	jr .jmp_4872

.jmp_488C
	cp a, $FE
	jr z, .jmp_4888
	ldh [$FF89], a
	ldh a, [$FF87]
	ld b, a
	ld a, [de]
	ld c, a
	ldh a, [$FF8B]
	bit 6, a
	jr nz, .jmp_48A3
	ldh a, [$FF90]
	add b
	adc c
	jr .jmp_48AD

.jmp_48A3
	ld a, b
	push af
	ldh a, [$FF90]
	ld b, a
	pop af
	sub b
	sbc c
	sbc a, $08
.jmp_48AD
	ldh [$FF93], a
	ldh a, [$FF88]
	ld b, a
	inc de
	ld a, [de]
	inc de
	ld c, a
	ldh a, [$FF8B]
	bit 5, a
	jr nz, .jmp_48C2
	ldh a, [$FF91]
	add b
	adc c
	jr .jmp_48CC

.jmp_48C2
	ld a, b
	push af
	ldh a, [$FF91]
	ld b, a
	pop af
	sub b
	sbc c
	sbc a, $08
.jmp_48CC
	ldh [$FF92], a
	push hl
	ldh a, [$FF8D]
	ld h, a
	ldh a, [$FF8E]
	ld l, a
	ldh a, [$FF95]
	and a
	jr z, .jmp_48DE
	ld a, $FF
	jr .jmp_48E0

.jmp_48DE
	ldh a, [$FF93]
.jmp_48E0
	ld [hli], a
	ldh a, [$FF92]
	ld [hli], a
	ldh a, [$FF89]
	ld [hli], a
	ldh a, [$FF94]
	ld b, a
	ldh a, [$FF8B]
	or b
	ld b, a
	ldh a, [$FF8A]
	or b
	ld [hli], a
	ld a, h
	ldh [$FF8D], a
	ld a, l
	ldh [$FF8E], a
	pop hl
	jp .jmp_4872

Call_03_48FC::
.jmp_48FC
	ld hl, $C209
	ld a, [hl]
	ld b, a
	and a
	ret z
	dec l
	ld a, [hl]
	cp a, $0F
	ret nc
	ld [hl], b
	inc l
	ld [hl], 0
	ret

Call_490D:: ; 490D
	ld a, [bc]			; C2x8
	ld e, a
	ld d, $00
	dec c
	ld a, [bc]			; C2x7
	dec c
	dec c
	dec c
	dec c
	dec c
	dec c
	and a
	ret z
	cp a, $02
	jr z, .jmp_4933
	add hl, de
	ld a, [hl]
	cp a, $7F
	jr z, .jmp_4948
	ld a, [bc]			; C2x1
	sub [hl]
	ld [bc], a
	inc e
.jmp_4929
	ld a, e
	inc c
	inc c
	inc c
	inc c
	inc c
	inc c
	inc c
	ld [bc], a			; C2x8
	ret

.jmp_4933
	ld a, e
	cp a, $FF
	jr z, .jmp_495B
	add hl, de
	ld a, [hl]
	cp a, $7F
	jr z, .jmp_4944
.jmp_493E
	ld a, [bc]			; C2x1
	add [hl]
	ld [bc], a
	dec e
	jr .jmp_4929

.jmp_4944
	dec hl
	dec e
	jr .jmp_493E

.jmp_4948
	dec de
	dec hl
	ld a, $02
	inc c
	inc c
	inc c
	inc c
	inc c
	inc c
	ld [bc], a
	dec c
	dec c
	dec c
	dec c
	dec c
	dec c
	jr .jmp_493E

.jmp_495B
	xor a
	inc c
	inc c
	inc c
	inc c
	inc c
	inc c
	ld [bc], a
	inc c
	ld [bc], a
	ret

; new
jr_003_4966:
    inc e                                         ; $4966: $1C
    ld a, [de]                                    ; $4967: $1A
    cp $0F                                        ; $4968: $FE $0F
    jr nc, jr_003_49B5                            ; $496A: $30 $49

    inc e                                         ; $496C: $1C
    dec a                                         ; $496D: $3D
    ld [de], a                                    ; $496E: $12
    dec e                                         ; $496F: $1D
    ld a, $0F                                     ; $4970: $3E $0F
    ld [de], a                                    ; $4972: $12
    jr jr_003_49B5                                ; $4973: $18 $40

jr_003_4975:
    push af                                       ; $4975: $F5
    ld a, [de]                                    ; $4976: $1A
    and a                                         ; $4977: $A7
    jr nz, jr_003_4988                            ; $4978: $20 $0E

    ld a, [wMarioMomentum]                        ; $497A: $FA $0C $C2
    cp $03                                        ; $497D: $FE $03
    ld a, $02                                     ; $497F: $3E $02
    jr c, jr_003_4985                             ; $4981: $38 $02

    ld a, $04                                     ; $4983: $3E $04

jr_003_4985:
    ld [wMarioRunning], a                         ; $4985: $EA $0E $C2

jr_003_4988:
    pop af                                        ; $4988: $F1
    jr jr_003_49AC                                ; $4989: $18 $21

Call_03_498B::
    ldh a, [hGameState]                                  ; $498B: $F0 $B3
    cp $0D                                        ; $498D: $FE $0D
    jp z, Jump_003_4A7F                           ; $498F: $CA $7F $4A

    ld de, wJumpStatus                            ; $4992: $11 $07 $C2
    ldh a, [hJoyPressed]                                  ; $4995: $F0 $81
    ld b, a                                       ; $4997: $47
    ldh a, [hJoyHeld]                                  ; $4998: $F0 $80
    bit 1, a                                      ; $499A: $CB $4F
    jr nz, jr_003_4975                            ; $499C: $20 $D7

    push af                                       ; $499E: $F5
    ld a, [wMarioRunning]                         ; $499F: $FA $0E $C2
    cp $04                                        ; $49A2: $FE $04
    jr nz, jr_003_49AB                            ; $49A4: $20 $05

    ld a, $02                                     ; $49A6: $3E $02
    ld [wMarioRunning], a                         ; $49A8: $EA $0E $C2

jr_003_49AB:
    pop af                                        ; $49AB: $F1

jr_003_49AC:
    bit 0, a                                      ; $49AC: $CB $47
    jr nz, jr_003_49BF                            ; $49AE: $20 $0F

    ld a, [de]                                    ; $49B0: $1A
    cp $01                                        ; $49B1: $FE $01
    jr z, jr_003_4966                             ; $49B3: $28 $B1

jr_003_49B5:
    bit 7, b                                      ; $49B5: $CB $78
    jp nz, Jump_003_4A77                          ; $49B7: $C2 $77 $4A

Jump_003_49BA:
    bit 1, b                                      ; $49BA: $CB $48
    jr nz, jr_003_49FD                            ; $49BC: $20 $3F

    ret                                           ; $49BE: $C9


jr_003_49BF:
    ld a, [de]                                    ; $49BF: $1A
    and a                                         ; $49C0: $A7
    jr nz, jr_003_49B5                            ; $49C1: $20 $F2

    ld hl, wMarioOnGround                         ; $49C3: $21 $0A $C2
    ld a, [hl]                                    ; $49C6: $7E
    and a                                         ; $49C7: $A7
    jr z, jr_003_49B5                             ; $49C8: $28 $EB

    bit 0, b                                      ; $49CA: $CB $40
    jr z, jr_003_49B5                             ; $49CC: $28 $E7

    ld [hl], $00                                  ; $49CE: $36 $00
    ld hl, wMarioAnimIndex                        ; $49D0: $21 $03 $C2
    push hl                                       ; $49D3: $E5
    ld a, [hl]                                    ; $49D4: $7E
    cp $18                                        ; $49D5: $FE $18
    jr z, jr_003_49F2                             ; $49D7: $28 $19

    and $F0                                       ; $49D9: $E6 $F0
    or $04                                        ; $49DB: $F6 $04
    ld [hl], a                                    ; $49DD: $77
    ld a, [wMarioRunning]                         ; $49DE: $FA $0E $C2
    cp $04                                        ; $49E1: $FE $04
    jr z, jr_003_49ED                             ; $49E3: $28 $08

    ld a, $02                                     ; $49E5: $3E $02
    ld [wMarioRunning], a                         ; $49E7: $EA $0E $C2
    ld [wC208], a                                 ; $49EA: $EA $08 $C2

jr_003_49ED:
    ld hl, wMarioMomentum                         ; $49ED: $21 $0C $C2
    ld [hl], $30                                  ; $49F0: $36 $30

jr_003_49F2:
    ld hl, wPlaySquareSFX                         ; $49F2: $21 $E0 $DF
    ld [hl], $01                                  ; $49F5: $36 $01
    ld a, $01                                     ; $49F7: $3E $01
    ld [de], a                                    ; $49F9: $12
    pop hl                                        ; $49FA: $E1
    jr jr_003_49B5                                ; $49FB: $18 $B8

jr_003_49FD:
    ld hl, wMarioMomentum                         ; $49FD: $21 $0C $C2
    ld a, [hl]                                    ; $4A00: $7E
    cp $06                                        ; $4A01: $FE $06
    jr nz, jr_003_4A0C                            ; $4A03: $20 $07

    ldh a, [$FF9F]                                  ; $4A05: $F0 $9F
    and a                                         ; $4A07: $A7
    jr nz, jr_003_4A0C                            ; $4A08: $20 $02

    ld [hl], $00                                  ; $4A0A: $36 $00

Jump_003_4A0C:
jr_003_4A0C:
    ldh a, [hGameState]                                  ; $4A0C: $F0 $B3
    cp $0D                                        ; $4A0E: $FE $0D
    ld b, $03                                     ; $4A10: $06 $03
    jr z, jr_003_4A1A                             ; $4A12: $28 $06

    ldh a, [$FFB5]                                  ; $4A14: $F0 $B5
    and a                                         ; $4A16: $A7
    ret z                                         ; $4A17: $C8

    ld b, $01                                     ; $4A18: $06 $01

jr_003_4A1A:
    ld hl, $FFA9                                  ; $4A1A: $21 $A9 $FF
    ld de, wOAMBuffer                             ; $4A1D: $11 $00 $C0

jr_003_4A20:
    ldi a, [hl]                                   ; $4A20: $2A
    and a                                         ; $4A21: $A7
    jr z, jr_003_4A2C                             ; $4A22: $28 $08

    inc e                                         ; $4A24: $1C
    inc e                                         ; $4A25: $1C
    inc e                                         ; $4A26: $1C
    inc e                                         ; $4A27: $1C
    dec b                                         ; $4A28: $05
    jr nz, jr_003_4A20                            ; $4A29: $20 $F5

    ret                                           ; $4A2B: $C9


jr_003_4A2C:
    push hl                                       ; $4A2C: $E5
    ld hl, wMarioFacingDir                        ; $4A2D: $21 $05 $C2
    ld b, [hl]                                    ; $4A30: $46
    ld hl, wMarioPosY                             ; $4A31: $21 $01 $C2
    ldi a, [hl]                                   ; $4A34: $2A
    add $FE                                       ; $4A35: $C6 $FE
    ld [de], a                                    ; $4A37: $12
    inc e                                         ; $4A38: $1C
    ld c, $02                                     ; $4A39: $0E $02
    bit 5, b                                      ; $4A3B: $CB $68
    jr z, jr_003_4A41                             ; $4A3D: $28 $02

    ld c, $F8                                     ; $4A3F: $0E $F8

jr_003_4A41:
    ldi a, [hl]                                   ; $4A41: $2A
    add c                                         ; $4A42: $81
    ld [de], a                                    ; $4A43: $12
    ld c, $60                                     ; $4A44: $0E $60
    inc e                                         ; $4A46: $1C
    ldh a, [hGameState]                                  ; $4A47: $F0 $B3
    cp $0D                                        ; $4A49: $FE $0D
    jr nz, jr_003_4A57                            ; $4A4B: $20 $0A

    ld c, $7A                                     ; $4A4D: $0E $7A
    ldh a, [hLevelIndex]                                  ; $4A4F: $F0 $E4
    cp $0B                                        ; $4A51: $FE $0B
    jr nz, jr_003_4A57                            ; $4A53: $20 $02

    ld c, $6E                                     ; $4A55: $0E $6E

jr_003_4A57:
    ld a, c                                       ; $4A57: $79
    ld [de], a                                    ; $4A58: $12
    inc e                                         ; $4A59: $1C
    xor a                                         ; $4A5A: $AF
    ld [de], a                                    ; $4A5B: $12
    pop hl                                        ; $4A5C: $E1
    dec l                                         ; $4A5D: $2D
    ld c, $0A                                     ; $4A5E: $0E $0A
    bit 5, b                                      ; $4A60: $CB $68
    jr nz, jr_003_4A66                            ; $4A62: $20 $02

    ld c, $09                                     ; $4A64: $0E $09

jr_003_4A66:
    ld [hl], c                                    ; $4A66: $71
    ld hl, wPlaySquareSFX                         ; $4A67: $21 $E0 $DF
    ld [hl], $02                                  ; $4A6A: $36 $02
    ld a, $0C                                     ; $4A6C: $3E $0C
    ld [$C0AE], a                                 ; $4A6E: $EA $AE $C0
    ld a, $FF                                     ; $4A71: $3E $FF
    ld [wSuperballTTL], a                         ; $4A73: $EA $A9 $C0
    ret                                           ; $4A76: $C9


Jump_003_4A77:
    ld hl, wMarioMomentum                         ; $4A77: $21 $0C $C2
    ld [hl], $20                                  ; $4A7A: $36 $20
    jp Jump_003_49BA                              ; $4A7C: $C3 $BA $49


Jump_003_4A7F:
    ldh a, [hJoyPressed]                                  ; $4A7F: $F0 $81
    and $03                                       ; $4A81: $E6 $03
    jr nz, jr_003_4A0C                            ; $4A83: $20 $87

    ldh a, [hJoyHeld]                                  ; $4A85: $F0 $80
    bit 0, a                                      ; $4A87: $CB $47
    ret z                                         ; $4A89: $C8

    ld hl, $C0AE                                  ; $4A8A: $21 $AE $C0
    ld a, [hl]                                    ; $4A8D: $7E
    and a                                         ; $4A8E: $A7
    jp z, Jump_003_4A0C                           ; $4A8F: $CA $0C $4A

    dec [hl]                                      ; $4A92: $35
    ret                                           ; $4A93: $C9

Call_03_4A94::
    ldh a, [$FF9F]                                  ; $4A94: $F0 $9F
    and a                                         ; $4A96: $A7
    ret z                                         ; $4A97: $C8

    cp $FF                                        ; $4A98: $FE $FF
    ret z                                         ; $4A9A: $C8

    ld a, [$C0D8]                                 ; $4A9B: $FA $D8 $C0
    and a                                         ; $4A9E: $A7
    jr z, .jr_003_4AA7                             ; $4A9F: $28 $06

    dec a                                         ; $4AA1: $3D
    ld [$C0D8], a                                 ; $4AA2: $EA $D8 $C0
    jr .jr_003_4AD1                                ; $4AA5: $18 $2A

.jr_003_4AA7
    ld a, [$C0DC]                                 ; $4AA7: $FA $DC $C0
    sla a                                         ; $4AAA: $CB $27
    ld e, a                                       ; $4AAC: $5F
    ld d, $00                                     ; $4AAD: $16 $00
    ld hl, Data_4AE4                              ; $4AAF: $21 $E4 $4A
    add hl, de                                    ; $4AB2: $19
    ld e, [hl]                                    ; $4AB3: $5E
    inc hl                                        ; $4AB4: $23
    ld d, [hl]                                    ; $4AB5: $56
    push de                                       ; $4AB6: $D5
    pop hl                                        ; $4AB7: $E1
    ld a, [$C0D9]                                 ; $4AB8: $FA $D9 $C0
    ld d, $00                                     ; $4ABB: $16 $00
    ld e, a                                       ; $4ABD: $5F
    add hl, de                                    ; $4ABE: $19
    ldi a, [hl]                                   ; $4ABF: $2A
    cp $FF                                        ; $4AC0: $FE $FF
    jr z, .jr_003_4ADE                             ; $4AC2: $28 $1A

    ld [$C0DA], a                                 ; $4AC4: $EA $DA $C0
    ld a, [hl]                                    ; $4AC7: $7E
    ld [$C0D8], a                                 ; $4AC8: $EA $D8 $C0
    inc e                                         ; $4ACB: $1C
    inc e                                         ; $4ACC: $1C
    ld a, e                                       ; $4ACD: $7B
    ld [$C0D9], a                                 ; $4ACE: $EA $D9 $C0

.jr_003_4AD1
    ldh a, [hJoyHeld]                                  ; $4AD1: $F0 $80
    ld [$C0DB], a                                 ; $4AD3: $EA $DB $C0
    ld a, [$C0DA]                                 ; $4AD6: $FA $DA $C0
    ldh [hJoyHeld], a                                  ; $4AD9: $E0 $80
    ldh [hJoyPressed], a                                  ; $4ADB: $E0 $81
    ret                                           ; $4ADD: $C9


.jr_003_4ADE
    xor a                                         ; $4ADE: $AF
    ld [$C0DA], a                                 ; $4ADF: $EA $DA $C0
    jr .jr_003_4AD1                                ; $4AE2: $18 $ED

Data_4AE4::
	db $50
	db $65
	db $18
	db $64
	db $9a
	db $64

Call_03_4AEA::
    ld b, $04                                     ; $4AEA: $06 $04
    ld de, $0010                                  ; $4AEC: $11 $10 $00
    ld hl, wEntityVisible                         ; $4AEF: $21 $10 $C2

jr_003_4AF2:
    push hl                                       ; $4AF2: $E5
    ld a, [hl]                                    ; $4AF3: $7E
    cp $80                                        ; $4AF4: $FE $80
    jr nz, jr_003_4AFA                            ; $4AF6: $20 $02

    ld [hl], $FF                                  ; $4AF8: $36 $FF

jr_003_4AFA:
    and a                                         ; $4AFA: $A7
    jr nz, jr_003_4B1B                            ; $4AFB: $20 $1E

    push de                                       ; $4AFD: $D5
    ld de, $0007                                  ; $4AFE: $11 $07 $00
    add hl, de                                    ; $4B01: $19
    pop de                                        ; $4B02: $D1
    ld a, [hl]                                    ; $4B03: $7E
    and a                                         ; $4B04: $A7
    jr z, jr_003_4B32                             ; $4B05: $28 $2B

    dec l                                         ; $4B07: $2D
    dec l                                         ; $4B08: $2D
    ld a, [hl]                                    ; $4B09: $7E
    dec l                                         ; $4B0A: $2D
    dec l                                         ; $4B0B: $2D
    dec l                                         ; $4B0C: $2D
    and a                                         ; $4B0D: $A7
    jr nz, jr_003_4B25                            ; $4B0E: $20 $15

    inc [hl]                                      ; $4B10: $34
    ldh a, [$FFF3]                                  ; $4B11: $F0 $F3
    ld c, a                                       ; $4B13: $4F
    ldh a, [hScrollX]                                  ; $4B14: $F0 $A4
    sub c                                         ; $4B16: $91
    ld c, a                                       ; $4B17: $4F
    ld a, [hl]                                    ; $4B18: $7E
    sub c                                         ; $4B19: $91
    ld [hl], a                                    ; $4B1A: $77

jr_003_4B1B:
    pop hl                                        ; $4B1B: $E1
    add hl, de                                    ; $4B1C: $19
    dec b                                         ; $4B1D: $05
    jr nz, jr_003_4AF2                            ; $4B1E: $20 $D2

    ldh a, [hScrollX]                                  ; $4B20: $F0 $A4
    ldh [$FFF3], a                                  ; $4B22: $E0 $F3
    ret                                           ; $4B24: $C9


jr_003_4B25:
    dec [hl]                                      ; $4B25: $35
    ldh a, [$FFF3]                                  ; $4B26: $F0 $F3
    ld c, a                                       ; $4B28: $4F
    ldh a, [hScrollX]                                  ; $4B29: $F0 $A4
    sub c                                         ; $4B2B: $91
    ld c, a                                       ; $4B2C: $4F
    ld a, [hl]                                    ; $4B2D: $7E
    sub c                                         ; $4B2E: $91
    ld [hl], a                                    ; $4B2F: $77
    jr jr_003_4B1B                                ; $4B30: $18 $E9

jr_003_4B32:
    pop hl                                        ; $4B32: $E1
    push hl                                       ; $4B33: $E5
    ld [hl], $80                                  ; $4B34: $36 $80
    inc l                                         ; $4B36: $2C
    inc l                                         ; $4B37: $2C
    ld [hl], $FF                                  ; $4B38: $36 $FF
    jr jr_003_4B1B                                ; $4B3A: $18 $DF

Call_03_4B3C::
    ldh a, [$FFEE]                                  ; $4B3C: $F0 $EE
    cp $03                                        ; $4B3E: $FE $03
    ret nz                                        ; $4B40: $C0

    ld hl, $C02D                                  ; $4B41: $21 $2D $C0
    ldh a, [hScrollX]                                  ; $4B44: $F0 $A4
    ld b, a                                       ; $4B46: $47
    ldh a, [$FFF2]                                  ; $4B47: $F0 $F2
    sub b                                         ; $4B49: $90
    ldd [hl], a                                   ; $4B4A: $32
    ld a, [wMarioPosY]                            ; $4B4B: $FA $01 $C2
    sub $0B                                       ; $4B4E: $D6 $0B
    ld [hl], a                                    ; $4B50: $77
    ld a, [wMarioOnGround]                        ; $4B51: $FA $0A $C2
    and a                                         ; $4B54: $A7
    jr nz, jr_003_4B62                            ; $4B55: $20 $0B

    ldh a, [$FFF1]                                  ; $4B57: $F0 $F1
    ld b, a                                       ; $4B59: $47
    sub $04                                       ; $4B5A: $D6 $04
    cp [hl]                                       ; $4B5C: $BE
    jr nc, jr_003_4B69                            ; $4B5D: $30 $0A

    ld a, b                                       ; $4B5F: $78
    cp [hl]                                       ; $4B60: $BE
    ret nc                                        ; $4B61: $D0

jr_003_4B62:
    ld [hl], $00                                  ; $4B62: $36 $00
    ld a, $04                                     ; $4B64: $3E $04
    ldh [$FFEE], a                                  ; $4B66: $E0 $EE
    ret                                           ; $4B68: $C9


jr_003_4B69:
    ld a, $02                                     ; $4B69: $3E $02
    ld [wJumpStatus], a                           ; $4B6B: $EA $07 $C2
    ret                                           ; $4B6E: $C9

Call_03_4B6F::
    ld hl, wMarioPosY                             ; $4B6F: $21 $01 $C2
    ld a, [hl]                                    ; $4B72: $7E
    cp $B4                                        ; $4B73: $FE $B4
    ret c                                         ; $4B75: $D8

    cp $C0                                        ; $4B76: $FE $C0
    ret nc                                        ; $4B78: $D0

    xor a                                         ; $4B79: $AF
    ldh [hSuperStatus], a                                  ; $4B7A: $E0 $99
    ldh [$FFB5], a                                  ; $4B7C: $E0 $B5
    inc a                                         ; $4B7E: $3C
    ldh [hGameState], a                                  ; $4B7F: $E0 $B3
    inc a                                         ; $4B81: $3C
    ld [wPlaySong], a                             ; $4B82: $EA $E8 $DF
    ld a, $90                                     ; $4B85: $3E $90
    ldh [hTimer], a                                  ; $4B87: $E0 $A6
    ret                                           ; $4B89: $C9

Call_03_4B8A::
    ldh a, [hSuperStatus]                                  ; $4B8A: $F0 $99
    cp $01                                        ; $4B8C: $FE $01
    ret nz                                        ; $4B8E: $C0

    ldh a, [hTimer]                                  ; $4B8F: $F0 $A6
    and a                                         ; $4B91: $A7
    jr z, jr_003_4BA4                             ; $4B92: $28 $10

    and $03                                       ; $4B94: $E6 $03
    ret nz                                        ; $4B96: $C0

    xor a                                         ; $4B97: $AF
    ld [wMarioVisible], a                         ; $4B98: $EA $00 $C2
    ld a, [wMarioAnimIndex]                       ; $4B9B: $FA $03 $C2
    xor $10                                       ; $4B9E: $EE $10
    ld [wMarioAnimIndex], a                       ; $4BA0: $EA $03 $C2
    ret                                           ; $4BA3: $C9


jr_003_4BA4:
    ld a, $02                                     ; $4BA4: $3E $02
    ldh [hSuperStatus], a                                  ; $4BA6: $E0 $99
    xor a                                         ; $4BA8: $AF
    ld [wMarioVisible], a                         ; $4BA9: $EA $00 $C2
    ld a, [wMarioAnimIndex]                       ; $4BAC: $FA $03 $C2
    or $10                                        ; $4BAF: $F6 $10
    ld [wMarioAnimIndex], a                       ; $4BB1: $EA $03 $C2
    ret                                           ; $4BB4: $C9

Call_03_4BB5::
    ldh a, [hSuperStatus]                                  ; $4BB5: $F0 $99
    cp $04                                        ; $4BB7: $FE $04
    jr z, jr_003_4BE0                             ; $4BB9: $28 $25

    cp $03                                        ; $4BBB: $FE $03
    ret nz                                        ; $4BBD: $C0

    ldh a, [hTimer]                                  ; $4BBE: $F0 $A6
    and a                                         ; $4BC0: $A7
    jr z, jr_003_4BCF                             ; $4BC1: $28 $0C

    and $03                                       ; $4BC3: $E6 $03
    ret nz                                        ; $4BC5: $C0

    ld a, [wMarioAnimIndex]                       ; $4BC6: $FA $03 $C2
    xor $10                                       ; $4BC9: $EE $10
    ld [wMarioAnimIndex], a                       ; $4BCB: $EA $03 $C2
    ret                                           ; $4BCE: $C9


jr_003_4BCF:
    ld a, $04                                     ; $4BCF: $3E $04
    ldh [hSuperStatus], a                                  ; $4BD1: $E0 $99
    ld a, $40                                     ; $4BD3: $3E $40
    ldh [hTimer], a                                  ; $4BD5: $E0 $A6
    ld a, [wMarioAnimIndex]                       ; $4BD7: $FA $03 $C2
    and $0F                                       ; $4BDA: $E6 $0F
    ld [wMarioAnimIndex], a                       ; $4BDC: $EA $03 $C2
    ret                                           ; $4BDF: $C9


jr_003_4BE0:
    ldh a, [hTimer]                                  ; $4BE0: $F0 $A6
    and a                                         ; $4BE2: $A7
    jr z, jr_003_4BF1                             ; $4BE3: $28 $0C

    and $03                                       ; $4BE5: $E6 $03
    ret nz                                        ; $4BE7: $C0

    ld a, [wMarioVisible]                         ; $4BE8: $FA $00 $C2
    xor $80                                       ; $4BEB: $EE $80
    ld [wMarioVisible], a                         ; $4BED: $EA $00 $C2
    ret                                           ; $4BF0: $C9


jr_003_4BF1:
    xor a                                         ; $4BF1: $AF
    ldh [hSuperStatus], a                                  ; $4BF2: $E0 $99
    ld [wMarioVisible], a                         ; $4BF4: $EA $00 $C2
    ld a, [wMarioAnimIndex]                       ; $4BF7: $FA $03 $C2
    and $0F                                       ; $4BFA: $E6 $0F
    ld [wMarioAnimIndex], a                       ; $4BFC: $EA $03 $C2
    ret                                           ; $4BFF: $C9


    ldh a, [$FF9F]                                  ; $4C00: $F0 $9F
    cp $FF                                        ; $4C02: $FE $FF
    ret nz                                        ; $4C04: $C0

    ldh a, [hJoyHeld]                                  ; $4C05: $F0 $80
    ld b, a                                       ; $4C07: $47
    ld a, [$C0DA]                                 ; $4C08: $FA $DA $C0
    cp b                                          ; $4C0B: $B8
    jr z, jr_003_4C2F                             ; $4C0C: $28 $21

    ld hl, $C300                                  ; $4C0E: $21 $00 $C3
    ld a, [$C0D9]                                 ; $4C11: $FA $D9 $C0
    ld e, a                                       ; $4C14: $5F
    ld d, $00                                     ; $4C15: $16 $00
    add hl, de                                    ; $4C17: $19
    ld a, [$C0DA]                                 ; $4C18: $FA $DA $C0
    ldi [hl], a                                   ; $4C1B: $22
    ld a, [$C0D8]                                 ; $4C1C: $FA $D8 $C0
    ld [hl], a                                    ; $4C1F: $77
    inc e                                         ; $4C20: $1C
    inc e                                         ; $4C21: $1C
    ld a, e                                       ; $4C22: $7B
    ld [$C0D9], a                                 ; $4C23: $EA $D9 $C0
    ld a, b                                       ; $4C26: $78
    ld [$C0DA], a                                 ; $4C27: $EA $DA $C0
    xor a                                         ; $4C2A: $AF
    ld [$C0D8], a                                 ; $4C2B: $EA $D8 $C0
    ret                                           ; $4C2E: $C9


jr_003_4C2F:
    ld a, [$C0D8]                                 ; $4C2F: $FA $D8 $C0
    inc a                                         ; $4C32: $3C
    ld [$C0D8], a                                 ; $4C33: $EA $D8 $C0
    ret                                           ; $4C36: $C9

Data_4C37::
	db $8d
	db $4c
	db $91
	db $4c
	db $95
	db $4c
	db $99
	db $4c
	db $9d
	db $4c
	db $a1
	db $4c
	db $a5
	db $4c
	db $a9
	db $4c
	db $8d
	db $4c
	db $91
	db $4c
	db $b1
	db $4c
	db $b5
	db $4c
	db $b9
	db $4c
	db $bd
	db $4c
	db $a5
	db $4c
	db $ad
	db $4c
	db $c1
	db $4c
	db $c5
	db $4c
	db $c9
	db $4c
	db $cd
	db $4c
	db $d1
	db $4c
	db $d5
	db $4c
	db $d9
	db $4c
	db $dd
	db $4c
	db $e1
	db $4c
	db $e5
	db $4c
	db $e9
	db $4c
	db $ed
	db $4c
	db $f1
	db $4c
	db $f5
	db $4c
	db $f5
	db $4c
	db $f5
	db $4c
	db $f9
	db $4c
	db $fd
	db $4c
	db $01
	db $4d
	db $05
	db $4d
	db $09
	db $4d
	db $0d
	db $4d
	db $19
	db $4d
	db $15
	db $4d
	db $11
	db $4d
	db $1d
	db $4d
	db $21
	db $4d
	db $25
	db $4d
	db $f9
	db $f8
	db $2c
	db $4d
	db $f9
	db $f8
	db $33
	db $4d
	db $f9
	db $f8
	db $3a
	db $4d
	db $f9
	db $f8
	db $41
	db $4d
	db $f9
	db $f8
	db $48
	db $4d
	db $f9
	db $f8
	db $4f
	db $4d
	db $f9
	db $fb
	db $56
	db $4d
	db $f9
	db $fb
	db $5d
	db $4d
	db $fc
	db $fc
	db $61
	db $4d
	db $f9
	db $f8
	db $68
	db $4d
	db $f9
	db $f8
	db $6f
	db $4d
	db $f9
	db $f8
	db $76
	db $4d
	db $f9
	db $f8
	db $7d
	db $4d
	db $f9
	db $f8
	db $84
	db $4d
	db $f9
	db $f8
	db $8b
	db $4d
	db $f9
	db $f8
	db $92
	db $4d
	db $f9
	db $f8
	db $99
	db $4d
	db $f9
	db $f8
	db $a0
	db $4d
	db $f9
	db $f8
	db $a7
	db $4d
	db $f9
	db $fb
	db $ae
	db $4d
	db $f9
	db $fb
	db $b5
	db $4d
	db $f9
	db $f8
	db $bc
	db $4d
	db $f9
	db $f8
	db $c3
	db $4d
	db $f9
	db $f8
	db $ca
	db $4d
	db $f9
	db $f8
	db $d1
	db $4d
	db $f9
	db $f8
	db $d8
	db $4d
	db $f9
	db $f8
	db $df
	db $4d
	db $f9
	db $f8
	db $e6
	db $4d
	db $f9
	db $f8
	db $ed
	db $4d
	db $f9
	db $f8
	db $f4
	db $4d
	db $f9
	db $f8
	db $fb
	db $4d
	db $f9
	db $f8
	db $02
	db $4e
	db $f9
	db $f8
	db $09
	db $4e
	db $f9
	db $f8
	db $14
	db $4e
	db $f9
	db $f7
	db $1f
	db $4e
	db $f9
	db $f8
	db $2a
	db $4e
	db $f9
	db $f8
	db $35
	db $4e
	db $f9
	db $f8
	db $44
	db $4e
	db $00
	db $01
	db $10
	db $11
	db $ff
	db $44
	db $4e
	db $02
	db $03
	db $12
	db $13
	db $ff
	db $44
	db $4e
	db $04
	db $05
	db $14
	db $15
	db $ff
	db $44
	db $4e
	db $00
	db $01
	db $16
	db $17
	db $ff
	db $44
	db $4e
	db $08
	db $09
	db $18
	db $19
	db $ff
	db $44
	db $4e
	db $0a
	db $0b
	db $1a
	db $1b
	db $ff
	db $44
	db $4e
	db $00
	db $01
	db $0c
	db $0d
	db $ff
	db $44
	db $4e
	db $00
	db $01
	db $1c
	db $1d
	db $ff
	db $44
	db $4e
	db $62
	db $ff
	db $44
	db $4e
	db $70
	db $71
	db $72
	db $73
	db $ff
	db $44
	db $4e
	db $70
	db $71
	db $74
	db $73
	db $ff
	db $44
	db $4e
	db $63
	db $64
	db $65
	db $66
	db $ff
	db $44
	db $4e
	db $63
	db $64
	db $65
	db $67
	db $ff
	db $44
	db $4e
	db $20
	db $21
	db $30
	db $31
	db $ff
	db $44
	db $4e
	db $22
	db $23
	db $32
	db $33
	db $ff
	db $44
	db $4e
	db $24
	db $25
	db $34
	db $35
	db $ff
	db $44
	db $4e
	db $22
	db $23
	db $36
	db $37
	db $ff
	db $44
	db $4e
	db $28
	db $29
	db $38
	db $39
	db $ff
	db $44
	db $4e
	db $2a
	db $2b
	db $3a
	db $3b
	db $ff
	db $44
	db $4e
	db $2c
	db $2d
	db $3c
	db $3d
	db $ff
	db $44
	db $4e
	db $2e
	db $2f
	db $3e
	db $3f
	db $ff
	db $44
	db $4e
	db $40
	db $41
	db $42
	db $43
	db $ff
	db $44
	db $4e
	db $44
	db $45
	db $46
	db $47
	db $ff
	db $44
	db $4e
	db $75
	db $76
	db $77
	db $78
	db $ff
	db $44
	db $4e
	db $75
	db $76
	db $79
	db $78
	db $ff
	db $44
	db $4e
	db $68
	db $69
	db $6a
	db $6b
	db $ff
	db $44
	db $4e
	db $68
	db $6c
	db $6a
	db $6d
	db $ff
	db $44
	db $4e
	db $a0
	db $a1
	db $b0
	db $b1
	db $ff
	db $44
	db $4e
	db $a2
	db $a3
	db $b2
	db $b3
	db $ff
	db $44
	db $4e
	db $4e
	db $49
	db $50
	db $51
	db $ff
	db $44
	db $4e
	db $48
	db $49
	db $4a
	db $4b
	db $ff
	db $44
	db $4e
	db $0c
	db $0d
	db $1c
	db $1d
	db $ff
	db $44
	db $4e
	db $2e
	db $2f
	db $3e
	db $3f
	db $ff
	db $5c
	db $4e
	db $2c
	db $2c
	db $4f
	db $3c
	db $2d
	db $3d
	db $4c
	db $4d
	db $ff
	db $4c
	db $4e
	db $0e
	db $4f
	db $2d
	db $4c
	db $1e
	db $3c
	db $3d
	db $4d
	db $ff
	db $5c
	db $4e
	db $26
	db $27
	db $4f
	db $3c
	db $2d
	db $3d
	db $4c
	db $4d
	db $ff
	db $5c
	db $4e
	db $fe
	db $7c
	db $61
	db $7d
	db $6f
	db $7e
	db $7b
	db $7f
	db $ff
	db $5c
	db $4e
	db $fe
	db $7c
	db $61
	db $7d
	db $6f
	db $7e
	db $61
	db $7d
	db $6f
	db $7e
	db $7b
	db $7f
	db $ff
	db $00
	db $00
	db $00
	db $08
	db $08
	db $00
	db $08
	db $08
	db $00
	db $00
	db $00
	db $09
	db $00
	db $11
	db $00
	db $19
	db $08
	db $00
	db $08
	db $09
	db $08
	db $11
	db $08
	db $19
	db $00
	db $00
	db $08
	db $00
	db $00
	db $08
	db $08
	db $08
	db $00
	db $10
	db $08
	db $10
	db $00
	db $18
	db $08
	db $18
	db $00
	db $20
	db $08
	db $20
	db $00
	db $28
	db $08
	db $28


SECTION "bank 3 levels", ROMX[$503F], BANK[3]
INCBIN "baserom.gb", $D03F, $6600 - $503F


StartSquareSoundTable::
	dw StartJumpSFX
	dw StartSuperballSFX
	dw StartStompSFX
	dw StartGrowSFX
	dw StartCoinSFX
	dw StartInjurySFX
	dw StartBumpSFX
	dw StartOneUpSFX
	dw StartGiraSFX
	dw StartTimerTickSFX
	dw StartFlowerSFX

ContinueSquareSoundTable::
	dw ContinueJumpSFX
	dw ContinueSquareSFX
	dw ContinueStompSFX
	dw ContinueSweepSquareSFX
	dw ContinueCoinSFX
	dw ContinueInjurySFX
	dw ContinueSquareSFX
	dw ContinueOneUpSFX
	dw ContinueSquareSFX
	dw ContinueSquareSFX
	dw ContinueSweepSquareSFX

; noise
StartNoiseSoundTable::
	dw StartExplosionSFX
	dw StartBrickShatterSFX
	dw StartDeathCrySFX
	dw StartFireBreathSFX

ContinueNoiseSoundTable::
	dw ContinueNoiseSFX
	dw ContinueNoiseSFX
	dw ContinueDeathCrySFX
	dw ContinueNoiseSFX

; dump_music.py
SongTable::
	dw Song_6F98	; 1
	dw Song_6FA3
	dw Song_6FAE
	dw Song_6FB9
	dw Song_6FC4	; 5
	dw Song_6FCF
	dw Song_6FDA
	dw Song_6FE5
	dw Song_78DC
	dw Song_78E7	; 10
	dw Song_78F2
	dw Song_78FD
	dw Song_7908
	dw Song_7913
	dw Song_791E	; 15
	dw Song_7929
	dw Song_7D6A
	dw Song_7934
	dw Song_793F	; 19

_Call_7FF0:: ; 6662
	push af
	push bc
	push de
	push hl
	ld a, 3		; Cartridge doesn't have RAM, and it's not even the correct
	ld [$00FF], a	; way to enable it. Bug
	ei				; Is this smart
	ldh a, [hPauseUnpauseMusic]
	cp a, 1		; 1 means pause music
	jr z, .pauseMusic
	cp a, 2		; 2 means unpause music
	jr z, .unpauseMusic
	ldh a, [hPauseTuneTimer]
	and a
	jr nz, .soundPaused
	ld c, $D3
	ld a, [$FF00+c]	; ?? To override other SFXs?
	and a
	jr z, .jmp_6688
	xor a
	ld [$FF00+c], a
	ld a, SFX_1UP		; 1UP sound
	ld [wPlaySquareSFX], a
.jmp_6688
	call PlaySquareSFX
	call PlayNoiseSFX	; play noise sfx
	call PlayWaveSFX	; play wave sfx
	call StartMusic
	call PlayMusic
	call PanStereo
.out
	xor a
	ld [wPlaySquareSFX], a
	ld [wPlaySong], a
	ld [$DFF0], a
	ld [wPlayNoiseSFX], a
	ldh [hPauseUnpauseMusic], a
	ld a, 7
	ld [$00FF], a	; Bug continued
	pop hl
	pop de
	pop bc
	pop af
	reti			; Interrupts are already enabled, necessary? Maybe to alert
					; peripherals to reset their interrupt flags
.pauseMusic
	call MuteSound
	xor a
	ld [wCurrentSquareSFX], a
	ld [wCurrentWaveSFX], a
	ld [wCurrentNoiseSFX], a
	ld a, $30
	ldh [hPauseTuneTimer], a

.playFirstNote
	ld hl, .pauseFirstNoteData

.playNote
	call SetupChannel.square2
	jr .out

.playSecondNote
	ld hl, .pauseSecondNoteData
	jr .playNote

.unpauseMusic
	xor a
	ldh [hPauseTuneTimer], a
	jr .jmp_6688

.soundPaused
	ld hl, hPauseTuneTimer
	dec [hl]
	ld a, [hl]
	cp a, $28				; Continue pause tune if it's still running
	jr z, .playSecondNote
	cp a, $20
	jr z, .playFirstNote
	cp a, $18
	jr z, .playSecondNote
	cp a, $10
	jr nz, .out
	inc [hl]				; Keep the hPauseTuneTimer non-zero, keeps the music
	jr .out					; paused

.pauseFirstNoteData
	db $B2, $E3, $83, $C7	; 1048.6 Hz ~ C6
.pauseSecondNoteData
	db $B2, $E3, $C1, $C7	; 2080.5 Hz ~ C7

PlayWaveSFX:: ; 66F6
	ld a, [$DFF0]	; SFX channel, only has boss cry
	cp a, $01
	jr z, .startBossCry
	ld a, [wCurrentWaveSFX]
	cp a, $01
	jr z, .continueBossCry
	ret

.bossCryChannelData
	db $80, $3A, $20, $B0, $C6	; 198/256 seconds, full volume, ~195 Hz (~G3)

.startBossCry
	ld [wCurrentWaveSFX], a
	ld hl, $DF3F
	set 7, [hl]			; stops the music from using the Wave channel?
	xor a
	ld [$DFF4], a
	ldh [rNR30], a		; wave DAC power
	ld hl, BossCryWavePattern
	call SetupWavePattern
	ldh a, [rDIV]		; supposedly random, but because the music code is
	and a, $1F			; linked to the timer interrupt, rDIV might always be 1
	ld b, a				; here? Bug?
	ld a, $D0
	add b
	ld [$DFF5], a		; "random" number from D0 to FF (always ~D1?)
	ld hl, .bossCryChannelData
	jp SetupChannel.wave	; copy HL to wave registers

.continueBossCry
	ldh a, [rDIV]
	and a, $07
	ld b, a				; "random" number from 0 to 7 (always ~2?)
	ld hl, $DFF4
	inc [hl]
	ld a, [hl]
	ld hl, $DFF5
	cp a, $0E
	jr nc, .lowerFreq
	inc [hl]
	inc [hl]
.writeFreq
	ld a, [hl]
	and a, $F8			; put a "random" number in the low nibble
	or b
	ld c, LOW(rNR33)	; modulate frequency
	ld [$FF00+c], a
	ret

.lowerFreq
	cp a, $1E
	jr z, .stopBossCry
	dec [hl]
	dec [hl]
	dec [hl]
	jr .writeFreq

.stopBossCry
	xor a
	ld [wCurrentWaveSFX], a
	ldh [rNR30], a
	ld hl, $DF3F
	res 7, [hl]			; release wave channel?
	ld bc, $DF36
	ld a, [bc]
	ld l, a
	inc c
	ld a, [bc]
	ld h, a
	jp SetupWavePattern

; no sweep, 50% duty, 1/16 second, envelope?, 1024 Hz, trigger, counter mode
TimerTickChannelData:: ; 6769
	db $00, $B0, $53, $80, $C7

StartTimerTickSFX:: ; 676E
	ld a, $03
	ld hl, TimerTickChannelData
	jp Jmp_69C6

GiraChannelData:: ; 6776
	db $3C, $80, $A0, $50, $84 ; 138.9 Hz, C#3

StartGiraSFX:: ; 677B
	call Call_6791.jmp_679C
	ret z
	ld a, $0E
	ld hl, GiraChannelData
	jp Jmp_69C6

JumpChannelData:: ; 6787
	db $00, $80, $D2, $0A, $86 ; 261.1 Hz C4

; Superball?
SuperballChannelData:: ; 678C
	db $3D, $80, $A3, $09, $87 ; 530.7 Hz ~ C5 (24 cents off...)

; used to check if new SFX overrides the one currently playing
Call_6791:: ; 6791
	ld a, [wCurrentSquareSFX]
	jr .jmp_67A2 ; WTF is this bullshit

.jmp_6796
	ld a, [wCurrentSquareSFX]
	cp a, SFX_STOMP
	ret z
.jmp_679C
	ld a, [wCurrentSquareSFX]
	cp a, SFX_COIN
	ret z
.jmp_67A2
	cp a, SFX_GROW
	ret z
	cp a, SFX_INJURY
	ret z
	cp a, SFX_1UP
	ret z
	cp a, SFX_FLOWER
	ret z
	ret

StartJumpSFX:: ; 67AF
	call Call_6791.jmp_6796
	ret z
	ld a, $10
	ld hl, JumpChannelData
	call Jmp_69C6
	ld hl, $DFE4
	ld [hl], $0A
	inc l
	ld [hl], $86
	ret

ContinueJumpSFX:: ; 67C4
	call UpdateSoundProgress
	and a
	jp z, StopSquareSFX
	ld hl, $DFE4
	ld e, [hl]
	inc l
	ld d, [hl]
	push hl
	ld hl, $000F		; some sort of sweep implementation
	add hl, de
	ld c, LOW(rNR13)
	ld a, l
	ld [$FF00+c], a
	ld b, a
	inc c
	ld a, h
	and a, $3F
	ld [$FF00+c], a		; rNR14
	pop hl
	ld [hld], a
	ld [hl], b
	ret

StartSuperballSFX:: ; 67E4
	call Call_6791.jmp_6796
	ret z
	ld a, $03
	ld hl, SuperballChannelData
	jp Jmp_69C6

ContinueSquareSFX::
	call UpdateSoundProgress
	and a
	ret nz

StopSquareSFX::
	xor a
	ld [wCurrentSquareSFX], a
	ldh [rNR10], a
	ld a, AUDENV_UP
	ldh [rNR12], a
	ld a, AUDHIGH_RESTART
	ldh [rNR14], a
	ld hl, $DF1F
	res 7, [hl]				; unlock
	ret

CoinChannelData1::
	db $00, $80, $E2, $06, $87 ; 524.3 Hz ~ C5
CoinChannelData2::
	db $00, $80, $E2, $83, $87 ; 1049 Hz ~ C6

StartCoinSFX:: ; 6813
	call Call_6791
	ret z
	ld hl, CoinChannelData1
	jp Jmp_69C6

ContinueCoinSFX:: ; 681D
	ld hl, $DFE4
	inc [hl]
	ld a, [hl]
	cp a, 4
	jr z, .secondTone
	cp a, $18
	jp z, StopSquareSFX
	ret

.secondTone
	ld hl, CoinChannelData2
	call SetupChannel.square1
	ret

StompChannelData1::
	db $57, $96, $8C, $30, $C7 ; 630 Hz
StompChannelData2::
	db $57, $96, $8C, $35, $C7 ; 645 Hz

StartStompSFX:: ; 683D
	call Call_6791.jmp_679C
	ret z
	ld a, 8
	ld hl, StompChannelData1
	jp Jmp_69C6

ContinueStompSFX:: ; 6849
	call UpdateSoundProgress
	and a
	ret nz
	ld hl, $DFE4
	ld a, [hl]
	inc [hl]
	cp a, 0
	jr z, .jmp_685D
	cp a, 1
	jp z, StopSquareSFX
	ret

.jmp_685D
	ld hl, StompChannelData2
	jp SetupChannel.square1

FlowerChannelData:: ; 6863
	db $54, $00, $9A, $20, $87 ; 585.1 Hz ~ D5 (though 721 is closer)

StartFlowerSFX:: ; 6868
	ld a, $60
	ld [$DFE6], a
	ld a, $05
	ld hl, FlowerChannelData
	jp Jmp_69C6

GrowChannelData::
	db $27, $80, $8A, $10, $86 ; 264.3 Hz ~ C4 (17 cents off though)

StartGrowSFX:: ; 687A
	ld a, $10
	ld [$DFE6], a
	ld a, $05
	ld hl, GrowChannelData
	jp Jmp_69C6

ContinueSweepSquareSFX::
	call UpdateSoundProgress
	and a
	ret nz
	ld hl, $DFE6
	ld a, $10				; sweep frequency up
	add [hl]
	ld [hl], a
	cp a, $E0
	jp z, StopSquareSFX
	ld c, LOW(rNR13)
	ld [$FF00+c], a
	inc c
	ld a, $86
	ld [$FF00+c], a			; rNR14
	ret

BumpChannelData:: ; 
	db $2C, $80, $D3, $40, $84 ; 136.5 Hz

StartBumpSFX:: ; 68A5
	call Call_6791.jmp_679C
	ret z
	ld a, $08
	ld hl, BumpChannelData
	jp Jmp_69C6

InjuryChannelData::
	db $3A, $80, $E3, $20, $86 ; 273.1 Hz (one octave above the bump thing?)

InjuryEnvelopeData::
	db $F3, $B3, $A3, $93, $83, $73, $63, $53, $43, $33, $23, $23, $13, $00

StartInjurySFX::
	ld a, [wCurrentSquareSFX]
	cp a, SFX_1UP
	ret z
	ld a, 6
	ld hl, InjuryChannelData
	jp Jmp_69C6

ContinueInjurySFX::
	call UpdateSoundProgress
	and a
	ret nz
	ld hl, $DFE4
	ld c, [hl]
	inc [hl]
	ld b, $00
	ld hl, InjuryEnvelopeData
	add hl, bc
	ld a, [hl]
	and a
	jp z, StopSquareSFX
	ld c, LOW(rNR12)
	ld [$FF00+c], a
	inc c
	inc c
	ld a, $87			; 609.6 Hz? Trigger
	ld [$FF00+c], a		; rNR14
	ret

StartOneUpSFX::
	ld a, $06
	ld hl, OneUpNote1
	jp Jmp_69C6

; Pentatonic riff. CDEGC'A in C major
OneUpNote1::
	db $00, $30, $F0, $A7, $C7 ; 1472 Hz ~ F#6

OneUpNote2::
	db $00, $30, $F0, $B1, $C7 ; 1659 Hz ~ G#6

OneUpNote3::
	db $00, $30, $F0, $BA, $C7 ; 1872 Hz ~ A#6

OneUpNote4::
	db $00, $30, $F0, $C4, $C7 ; 2184 Hz ~ C#7 (25 cents flat, bug, 7C5 is closer)

OneUpNote5::
	db $00, $30, $F0, $D4, $C7 ; 2978 Hz ~ F#7 (11 cents sharp)

OneUpNote6::
	db $00, $30, $F0, $CB, $C7 ; 2473 Hz ~ D#7 (11 cents flat)

ContinueOneUpSFX:: ; 6916
	call UpdateSoundProgress
	and a
	ret nz
	ld a, [$DFE4]		; when sound has ended, increment and load a new note
	inc a
	ld [$DFE4], a
	cp a, $01
	jr z, .playNote2
	cp a, $02
	jr z, .playNote3
	cp a, $03
	jr z, .playNote4
	cp a, $04
	jr z, .playNote5
	cp a, $05
	jr z, .playNote6
	jp StopSquareSFX

.playNote2
	ld hl, OneUpNote2
	jr .playNote

.playNote3
	ld hl, OneUpNote3
	jr .playNote

.playNote4
	ld hl, OneUpNote4
	jr .playNote

.playNote5
	ld hl, OneUpNote5
	jr .playNote

.playNote6
	ld hl, OneUpNote6
.playNote
	jp SetupChannel.square1

ExplosionChannelData:: ; 6953
	db $00, $F4, $57, $80

StartExplosionSFX:: ; 6957
	ld a, $30
	ld hl, ExplosionChannelData
	jp Jmp_69C6

Call_695F:: ; 695F
	ld a, [wCurrentNoiseSFX]
	cp a, SFX_EXPLOSION
	ret z
	ret

DeathCryChannelData:: ; 6966
	db $00, $2C, $1E, $80

; Death cry envelope of sorts? TODO
; first nibble: clock shift, increasing, 
; second nibble: width (always narrow), divisor code
Data_696A:: ; 696A
	db $1F, $2D, $2F, $3D, $3F, $00

StartDeathCrySFX:: ; 6970
	call Call_695F
	ret z
	ld a, $06
	ld hl, DeathCryChannelData
	jp Jmp_69C6

ContinueDeathCrySFX:: ; 697C
	call UpdateSoundProgress
	and a
	ret nz
	ld hl, $DFFC
	ld c, [hl]
	inc [hl]
	ld b, $00
	ld hl, Data_696A
	add hl, bc
	ld a, [hl]
	and a
	jr z, StopNoiseSFX
	ldh [rNR43], a		; noise characteristics
	ret

FireBreathChannelData:: ; 6993
	db $00, $6D, $54, $80

StartFireBreathSFX:: ; 6997
	ld a, $16
	ld hl, FireBreathChannelData
	jp Jmp_69C6

BlockShatterChannelData:: ; 699F
	db $00, $F2, $55, $80

StartBrickShatterSFX:: ; 69A3
	call Call_695F
	ret z
	ld a, $15
	ld hl, BlockShatterChannelData
	jp Jmp_69C6

ContinueNoiseSFX:: ; 69AF
	call UpdateSoundProgress
	and a
	ret nz

StopNoiseSFX::
	xor a
	ld [wCurrentNoiseSFX], a
	ld a, $08
	ldh [rNR42], a		; mute noise channel, setup envelope?
	ld a, $80
	ldh [rNR44], a		; trigger noise, consecutive mode
	ld hl, $DF4F		; noise?
	res 7, [hl]
	ret

; write SFX data to the channels (from where?)
; DE starts at DFxx + 2 (DFE2, DFFA, like that)
Jmp_69C6:: ; 69C6
	push af
	dec e			; using DFE- as example, but it holds for other engines as well
	ldh a, [$FFD1]
	ld [de], a		; DFE1 - currently playing sound
	inc e
	pop af
	inc e
	ld [de], a		; DFE3 - sound duration
	dec e
	xor a
	ld [de], a		; DFE2 - position
	inc e
	inc e
	ld [de], a		; DFE4
	inc e
	ld [de], a		; DFE5
	ld a, e
	cp a, $E5
	jr z, SetupChannel.square1
	cp a, $F5
	jr z, SetupChannel.wave
	cp a, $FD
	jr z, SetupChannel.noise
	ret

; copy HL to channel registers
SetupChannel:: ; 69E5
.square1
	push bc
	ld c, LOW(rNR10)
	ld b, $05
	jr .loopCopy

.square2
	push bc
	ld c, LOW(rNR21)
	ld b, $04
	jr .loopCopy

.wave
	push bc
	ld c, LOW(rNR30)
	ld b, $05
	jr .loopCopy

.noise
	push bc
	ld c, LOW(rNR41)
	ld b, $04
.loopCopy				; copy B bytes from HL to the channel in C
	ld a, [hli]
	ld [$FF00+c], a
	inc c
	dec b
	jr nz, .loopCopy
	pop bc
	ret

; do a lookup in HL
LookupSoundPointer:: ; 6A07
	inc e
	ldh [$FFD1], a
.lookup
	inc e
	dec a				; go from 1-based to 0-based
	sla a				; a pointer is two bytes
	ld c, a
	ld b, $00
	add hl, bc
	ld c, [hl]
	inc hl
	ld b, [hl]			; load in BC
	ld l, c
	ld h, b
	ld a, h				; HL ← BC
	ret

UpdateSoundProgress:: ;; 6A19
	push de			; DE is DFE0(or other) + 2 here
	ld l, e
	ld h, d			; HL ← DE
	inc [hl]		; increment position
	ld a, [hli]
	cp [hl]			; and compare with sound length
	jr nz, .out
	dec l			; end of sound, return zero
	xor a
	ld [hl], a
.out
	pop de
	ret

; copy HL to wave pattern
SetupWavePattern:: ; 6A26
	push bc
	ld c, $30		; FF30 Wave pattern RAM
.loop
	ld a, [hli]
	ld [$FF00+c], a
	inc c
	ld a, c
	cp a, $40
	jr nz, .loop
	pop bc
	ret

_InitSound:: ; 6A33
	xor a
	ld [wCurrentSquareSFX], a
	ld [wCurrentSong], a
	ld [wCurrentWaveSFX], a
	ld [wCurrentNoiseSFX], a	; currently playing music and sfx
	ld [$DF1F], a
	ld [$DF2F], a
	ld [$DF3F], a
	ld [$DF4F], a
	ldh [hPauseUnpauseMusic], a
	ldh [$FFDE], a
	ld a, $FF
	ldh [rNR51], a	; enable all channels to both outputs

MuteSound::
	ld a, 8
	ldh [rNR12], a
	ldh [rNR22], a
	ldh [rNR42], a	; volume to zero, enable envelope?
	ld a, AUDHIGH_RESTART
	ldh [rNR14], a
	ldh [rNR24], a
	ldh [rNR44], a	; restart sound, counter mode
	xor a
	ldh [rNR10], a	; disable sweep
	ldh [rNR30], a	; disable wave
	ret

PlaySquareSFX:: ; 6A6A
	ld de, wPlaySquareSFX			; non zero to start sfx
	ld a, [de]
	and a
	jr z, .continue			; if zero, play the current sound
	cp a, 12
	jr nc, .continue		; bounds check, 11 sound effects in this "channel"
	ld hl, $DF1F			; lock square channel 1
	set 7, [hl]
	ld hl, StartSquareSoundTable
	call LookupSoundPointer	; also sets FFD1
	jp hl

.continue
	inc e
	ld a, [de]
	and a
	jr z, .out
	ld hl, ContinueSquareSoundTable
	call LookupSoundPointer.lookup
	jp hl

.out					; RET Z? Bug
	ret

PlayNoiseSFX:: ; 6A8E
	ld de, wPlayNoiseSFX
	ld a, [de]
	and a
	jr z, .continue
	cp a, 5
	jr nc, .continue
	ld hl, $DF4F
	set 7, [hl]			; lock noise channel
	ld hl, StartNoiseSoundTable
	call LookupSoundPointer
	jp hl

.continue
	inc e
	ld a, [de]
	and a
	jr z, .out
	ld hl, ContinueNoiseSoundTable
	call LookupSoundPointer.lookup
	jp hl

.out				; forgot about RET Z? Bug
	ret

_Unreachable ; 6AB2
	jp _InitSound

StartMusic:: ; 6AB5
	ld hl, wPlaySong
	ld a, [hli]
	and a
	ret z
	cp a, $14
	ret nc				; 19 songs, bounds check
	ld [hl], a			; DFE9
	cp a, $FF			; Can never be true. Possibly -1 was used as a sentinel
	jr z, _Unreachable	; to stop all sound, like in Pokemon
	ld b, a
	ld hl, SongTable
	ld a, b
	and a, $1F			; Huh?
	call LookupSoundPointer.lookup
	call Call_6B8C
	jp .initStereo		; Weird

.initStereo
	ld a, [wCurrentSong]
	ld hl, StereoData
.loop
	dec a
	jr z, .writeStereoData
	inc hl
	inc hl
	inc hl
	inc hl
	jr .loop

.writeStereoData
	ld a, [hli]
	ldh [hMonoOrStereo], a
	ld a, [hli]
	ldh [hPanInterval], a
	ld a, [hli]
	ldh [hChannelEnableMask1], a
	ldh [rNR51], a
	ld a, [hli]
	ldh [hChannelEnableMask2], a
	xor a
	ldh [hPanTimer], a
	ldh [hPanCounter], a
	ret

PanExplosion:: ; 6AF6
	ld a, [wCurrentNoiseSFX]
	cp a, SFX_EXPLOSION		; Hmh?
	ret nz
	ld a, [hl]				; Always FFD5. Bug
	bit 1, a				; Every two ticks?
	ld a, $F7				; Noise left enable
	jr z, .applyMask
	ld a, $7F				; Noise right enable
.applyMask
	call ApplyChannelEnableMasks
.ret
	ret

PanStereo:: ; 6B09
	ld a, [wCurrentSong]
	and a
	ret z
	ldh a, [hMonoOrStereo]
	cp a, 1					; 01 = Mono, 02 = Stereo
	jr z, PanExplosion.ret	; Bug... RET Z
	ld hl, hPanTimer
	call PanExplosion		; Skips over following code if explosion is playing
	inc [hl]				; FFD5 - hPanTimer
	ld a, [hli]
	cp [hl]					; FFD6 - hPanInterval
	ret nz
	dec l
	ld [hl], $00			; FFD5 - hPanTimer
	inc l
	inc l
	inc [hl]				; FFD7 - hPanCounter
	ldh a, [hChannelEnableMask1]
	bit 0, [hl]				; Counter is only used for its lowest bit... Bug
	jr z, ApplyChannelEnableMasks
	ldh a, [hChannelEnableMask2]

ApplyChannelEnableMasks::
	ldh [rNR51], a
	ret

; Four bytes:
; 1: hMonoOrStereo - 01 if mono, 02 if stereo
; 2: hPanInterval - Ticks between panning
; 3: hChannelEnableMask1
; 4: hChannelEnableMask2
; Quite some unused masks. Bug?
StereoData:: ; 6B2F
	db $02, $24, $ED, $DE	; Music 1
	db $01, $18, $BD, $00
	db $02, $20, $7F, $B7
	db $01, $18, $ED, $7F
	db $01, $18, $FF, $F7	; Music 5
	db $02, $40, $7F, $F7
	db $02, $40, $7F, $F7
	db $01, $18, $FF, $F7
	db $01, $10, $FF, $A5
	db $01, $00, $65, $00	; Music 10
	db $01, $00, $FF, $00
	db $02, $08, $7F, $B5
	db $01, $00, $ED, $00
	db $01, $00, $ED, $00
	db $01, $00, $FF, $00	; Music 15
	db $01, $00, $ED, $00
	db $02, $18, $7E, $E7
	db $01, $18, $ED, $E7
	db $01, $00, $DE, $00	; Music 19

; copy 2 bytes from the address at HL to DE
CopyPointerIndirect:: ; 6B7B
	ld a, [hli]
	ld c, a
	ld a, [hl]
	ld b, a
	ld a, [bc]
	ld [de], a
	inc e
	inc bc
	ld a, [bc]
	ld [de], a
	ret

; copy 2 bytes from HL to DE
CopyPointer:: ; 6B86
	ld a, [hli]
	ld [de], a
	inc e
	ld a, [hli]
	ld [de], a
	ret

; setup array of addresses and such at DF00-DF4F
; HL contains the pointer from SongTable
Call_6B8C::; 6B8C
	call MuteSound
	xor a
	ldh [hPanTimer], a
	ldh [$FFD7], a
	ld de, $DF00
	ld b, $00
	ld a, [hli]
	ld [de], a				; byte 0 goes into DF00 (unused, always zero?)
	inc e
	call CopyPointer			; byte 1,2 into DF01, DF02
	ld de, $DF10
	call CopyPointer			; byte 3,4 DF10
	ld de, $DF20
	call CopyPointer			; byte 5,6 DF20
	ld de, $DF30
	call CopyPointer			; byte 7,8 DF30
	ld de, $DF40
	call CopyPointer			; byte 9,10 DF40
	ld hl, $DF10			; 
	ld de, $DF14
	call CopyPointerIndirect
	ld hl, $DF20
	ld de, $DF24
	call CopyPointerIndirect
	ld hl, $DF30
	ld de, $DF34
	call CopyPointerIndirect
	ld hl, $DF40
	ld de, $DF44
	call CopyPointerIndirect
	ld bc, $0410		; 4 loops, $10 bytes between each DFx2
	ld hl, $DF12
.loop
	ld [hl], $01		; set them all to 1
	ld a, c
	add l
	ld l, a
	dec b
	jr nz, .loop
	xor a
	ld [$DF1E], a
	ld [$DF2E], a
	ld [$DF3E], a
	ret

Jmp_6BF4 ; 6BF4
	push hl
	xor a
	ldh [rNR30], a		; disable wave channel
	ld l, e
	ld h, d
	call SetupWavePattern
	pop hl
	jr Jmp_6C00.jmp_6C2A

Jmp_6C00 ; 6C00
	call IncrementPointer
	call LoadFromHLindirect
	ld e, a
	call IncrementPointer
	call LoadFromHLindirect
	ld d, a
	call IncrementPointer
	call LoadFromHLindirect
	ld c, a
	inc l
	inc l
	ld [hl], e					; DFx6- will go into NRx2
	inc l
	ld [hl], d					; DFx7 - always 0 or 6F
	inc l
	ld [hl], c					; DFx8 - will go into NRx1
	dec l
	dec l
	dec l
	dec l
	push hl
	ld hl, hCurrentChannel		; unnecessary, it's in HRAM. Bug
	ld a, [hl]
	pop hl
	cp a, 3						; wave channel
	jr z, Jmp_6BF4
.jmp_6C2A
	call IncrementPointer
	jp PlayMusic.jmp_6CD7

; increment the address located at HL
IncrementPointer:: ; 6C30
	push de
	ld a, [hli]
	ld e, a
	ld a, [hld]
	ld d, a
	inc de
.storeDE
	ld a, e
	ld [hli], a
	ld a, d
	ld [hld], a
	pop de
	ret

IncrementPointerTwice:: ; 6C3C
	push de
	ld a, [hli]
	ld e, a
	ld a, [hld]
	ld d, a
	inc de
	inc de
	jr IncrementPointer.storeDE

; todo name
LoadFromHLindirect:: ; 6C45
	ld a, [hli]
	ld c, a
	ld a, [hld]
	ld b, a				; BC ← [HL]
	ld a, [bc]			; A ← [BC]
	ld b, a				; B ← [BC]
	ret

Jmp_6C4C
	pop hl
	jr .jmp_6C7A

.jmp_6C4F
	ldh a, [hCurrentChannel]
	cp a, 3			; wave
	jr nz, .jmp_6C65
	ld a, [$DF38]
	bit 7, a
	jr z, .jmp_6C65
	ld a, [hl]			; DF32?
	cp a, 6
	jr nz, .jmp_6C65
	ld a, $40
	ldh [rNR32], a		; wave channel volume
.jmp_6C65
	push hl
	ld a, l
	add a, 9
	ld l, a
	ld a, [hl]			; DFxB
	and a
	jr nz, Jmp_6C4C
	ld a, l
	add a, 4
	ld l, a
	bit 7, [hl]			; DFxF lock status
	jr nz, Jmp_6C4C		; jump if locked
	pop hl
	call PlayMusic.jmp_6DDC
.jmp_6C7A
	dec l
	dec l
	jp PlayMusic.nextChannel

.jmp_6C7F
	dec l
	dec l
	dec l
	dec l
	call IncrementPointerTwice
.jmp_6C86
	ld a, l
	add a, 4
	ld e, a
	ld d, h
	call CopyPointerIndirect
	cp a, $00
	jr z, .stopSong
	cp a, $FF
	jr z, .restartChannel
	inc l
	jp PlayMusic.jmp_6CD5

.restartChannel
	dec l
	push hl
	call IncrementPointerTwice		; skip over FFFF
	call LoadFromHLindirect
	ld e, a
	call IncrementPointer
	call LoadFromHLindirect
	ld d, a						; DE is restart pointer
	pop hl
	ld a, e
	ld [hli], a					; put it in DFx0-DFx1
	ld a, d
	ld [hld], a
	jr .jmp_6C86

.stopSong
	ld hl, wCurrentSong
	ld [hl], $00
	ld a, $FF
	ldh [rNR51], a
	call MuteSound
	ret

PlayMusic:: ; 6CBE
	ld hl, wCurrentSong				; music currently playing
	ld a, [hl]
	and a
	ret z
	ld a, 1						; start from channel 1, square (with sweep)
	ldh [hCurrentChannel], a
	ld hl, $DF10
.jmp_6CCB
	inc l
	ld a, [hli]					; DFx1
	and a
	jp z, Jmp_6C4C.jmp_6C7A
	dec [hl]					; DFx2
	jp nz, Jmp_6C4C.jmp_6C4F
.jmp_6CD5
	inc l
	inc l
.jmp_6CD7
	call LoadFromHLindirect		; DFx4 - DFx5
	cp a, $00					; stores a copy in B
	jp z, Jmp_6C4C.jmp_6C7F
	cp a, $9D
	jp z, Jmp_6C00
	and a, $F0
	cp a, $A0					; set note length?
	jr nz, .jmp_6D04
	ld a, b
	and a, $0F
	ld c, a
	ld b, $00
	push hl
	ld de, $DF01
	ld a, [de]
	ld l, a
	inc de
	ld a, [de]
	ld h, a						; HL ← note lengths pointer?
	add hl, bc
	ld a, [hl]
	pop hl
	dec l
	ld [hli], a					; DFx3
	call IncrementPointer
	call LoadFromHLindirect		; DFx4 - DFx5
.jmp_6D04
	ld a, b
	ld c, a
	ld b, $00
	call IncrementPointer		; DFx4 - DFx5
	ldh a, [hCurrentChannel]
	cp a, 4						; Noise
	jp z, .jmp_6D34
	push hl
	ld a, l
	add a, 5
	ld l, a
	ld e, l
	ld d, h						; DFx9
	inc l
	inc l
	ld a, c
	cp a, 1
	jr z, .jmp_6D2F
	ld [hl], 0					; DFxB
	ld hl, NotePitches
	add hl, bc
	ld a, [hli]
	ld [de], a					; DFx9
	inc e
	ld a, [hl]
	ld [de], a					; DFxA
	pop hl
	jp .jmp_6D4B

.jmp_6D2F
	ld [hl], $01
	pop hl
	jr .jmp_6D4B

.jmp_6D34						; Noisy stuff
	push hl
	ld de, $DF46				; volume (envelope)?
	ld hl, Data_6F06
	add hl, bc
.loop
	ld a, [hli]
	ld [de], a
	inc e
	ld a, e
	cp a, $4B
	jr nz, .loop
	ld c, LOW(rNR41)
	ld hl, $DF44
	jr .jmp_6D78

.jmp_6D4B
	push hl
	ldh a, [hCurrentChannel]
	cp a, 1					; Square 1
	jr z, .jmp_6D73
	cp a, 2					; Square 2
	jr z, .jmp_6D6F
	ld c, LOW(rNR30)		; Wave
	ld a, [$DF3F]			; Lock
	bit 7, a
	jr nz, .jmp_6D64
	xor a
	ld [$FF00+c], a
	ld a, $80
	ld [$FF00+c], a
.jmp_6D64
	inc c
	inc l
	inc l
	inc l
	inc l
	ld a, [hli]
	ld e, a
	ld d, $00
	jr .jmp_6D84

.jmp_6D6F
	ld c, LOW(rNR21)
	jr .jmp_6D78

.jmp_6D73
	ld c, LOW(rNR10)
	ld a, $00
	inc c
.jmp_6D78
	inc l
	inc l
	inc l
	ld a, [hld]				; DFx7
	and a
	jr nz, .jmp_6DCE
	ld a, [hli]				; DFx6
	ld e, a
.jmp_6D81
	inc l
	ld a, [hli]				; DFx8
	ld d, a
.jmp_6D84
	push hl
	inc l
	inc l
	ld a, [hli]				; DFxB
	and a
	jr z, .jmp_6D8D
	ld e, $08
.jmp_6D8D
	inc l
	inc l
	ld [hl], $00			; DFxE
	inc l
	ld a, [hl]				; DFxF lock
	pop hl
	bit 7, a
	jr nz, .resetNoteTimer	; don't update the channel registers when locked
	ld a, d
	ld [$FF00+c], a			; NRx1
	inc c
	ld a, e
	ld [$FF00+c], a			; NRx2
	inc c
	ld a, [hli]				; DFx9 freq lo
	ld [$FF00+c], a			; NRx3
	inc c
	ld a, [hl]				; DFxA freq hi
	or a, $80				; trigger
	ld [$FF00+c], a			; NRx4
	ld a, l
	or a, $05				; euhm
	ld l, a
	res 0, [hl]				; DFxF lock? lowest bit
.resetNoteTimer
	pop hl
	dec l
	ld a, [hld]				; DFx4 - note length
	ld [hld], a				; DFx3 - note timer
	dec l
.nextChannel
	ld de, hCurrentChannel
	ld a, [de]
	cp a, 4
	jr z, .jmp_6DC1			; done after 4 channels
	inc a
	ld [de], a
	ld de, $0010
	add hl, de
	jp .jmp_6CCB

.jmp_6DC1
	ld hl, $DF1E
	inc [hl]
	ld hl, $DF2E
	inc [hl]
	ld hl, $DF3E
	inc [hl]
	ret

.jmp_6DCE
	ld b, $00
	inc l
	jr .jmp_6D81

.call_6DD3
	ld a, b
	srl a
	ld l, a
	ld h, $00
	add hl, de
	ld e, [hl]
	ret

.jmp_6DDC
	push hl
	ld a, l
	add a, $06
	ld l, a
	ld a, [hl]				; DFx8
	and a, $0F
	jr z, .jmp_6DFC
	ldh [$FFD1], a
	ldh a, [hCurrentChannel]
	ld c, LOW(rNR13)
	cp a, $01				; square 1
	jr z, .jmp_6DFE
	ld c, LOW(rNR23)
	cp a, $02				; square 2
	jr z, .jmp_6DFE
	ld c, LOW(rNR33)
	cp a, $03				; wave
	jr z, .jmp_6DFE
.jmp_6DFC
	pop hl
	ret

.jmp_6DFE
	inc l
	ld a, [hli]				; DFx9 freq lo
	ld e, a
	ld a, [hl]				; DFxA freq hi
	ld d, a
	push de					; DE ← frequency
	ld a, l
	add a, $04
	ld l, a
	ld b, [hl]				; DFxE
	ldh a, [$FFD1]
	cp a, $01				; wut
	jr .jmp_6E18			; huh?

.jmp_6E0F
	cp a, $03
	jr .jmp_6E13			; huh?

.jmp_6E13
	ld hl, $FFFF
	jr .jmp_6E34

.jmp_6E18
	ld de, Data_6E3D
	call .call_6DD3			; loads the value at DE + B/2 into E
	bit 0, b				; if B is even
	jr nz, .jmp_6E24
	swap e					; select high nibble
.jmp_6E24
	ld a, e
	and a, $0F				; lower nibble
	bit 3, a
	jr z, .jmp_6E31
	ld h, $FF
	or a, $F0
	jr .jmp_6E33

.jmp_6E31
	ld h, $00
.jmp_6E33
	ld l, a
.jmp_6E34
	pop de
	add hl, de
	ld a, l
	ld [$FF00+c], a		; freq lo
	inc c
	ld a, h
	ld [$FF00+c], a		; freq hi
	jr .jmp_6DFC

Data_6E3D:: ; 6E3D
	db $00, $00, $00, $00, $00
	db $00, $10, $00, $0F, $00, $00, $11, $00, $0F, $F0
	db $01, $12, $10, $FF, $EF, $01, $12, $10, $FF, $EF
	db $01, $12, $10, $FF, $EF, $01, $12, $10, $FF, $EF
	db $01, $12, $10, $FF, $EF, $01, $12, $10, $FF, $EF
	db $01, $12, $10, $FF, $EF, $01, $12, $10, $FF, $EF

; Chromatic pitches. The number in brackets shows how many cents the pitch
; is off from ideal
NotePitches:: ; 6E74
	dw $F00 ; Bug? Functions as $700, 512 Hz (no note)
	dw $02C ;   65.4 Hz C 2 (-0)
	dw $09C ;   69.3 Hz C#2 (-0) Bug! 09D is closer
	dw $106 ;   73.4 Hz D 2 (-1) Bug! 107 is closer
	dw $16B ;   77.8 Hz D#2 (+0)
	dw $1C9 ;   82.4 Hz E 2 (-0)
	dw $223 ;   87.3 Hz F 2 (+0)
	dw $277 ;   92.5 Hz F#2 (+0)
	dw $2C6 ;   98.0 Hz G 2 (-1) Bug! 2C7 is closer
	dw $312 ;  103.9 Hz G#2 (+1)
	dw $356 ;  109.8 Hz A 2 (-4) Bug! 357 is closer
	dw $39B ;  116.5 Hz A#2 (-0)
	dw $3DA ;  123.4 Hz B 2 (-1)
	dw $416 ;  130.8 Hz C 3 (-0)
	dw $44E ;  138.6 Hz C#3 (-0)
	dw $483 ;  146.8 Hz D 3 (-1)
	dw $4B5 ;  155.5 Hz D#3 (-1)
	dw $4E5 ;  164.9 Hz E 3 (+1)
	dw $511 ;  174.5 Hz F 3 (-1)
	dw $53B ;  184.9 Hz F#3 (-1)
	dw $563 ;  195.9 Hz G 3 (-1)
	dw $589 ;  207.7 Hz G#3 (+1)
	dw $5AC ;  219.9 Hz A 3 (-1)
	dw $5CE ;  233.2 Hz A#3 (+1)
	dw $5ED ;  246.8 Hz B 3 (-1)
	dw $60A ;  261.1 Hz C 4 (-3) Bug! 60B is closer
	dw $627 ;  277.1 Hz C#4 (-0)
	dw $642 ;  293.9 Hz D 4 (+1)
	dw $65B ;  311.3 Hz D#4 (+1)
	dw $672 ;  329.3 Hz E 4 (-2)
	dw $689 ;  349.5 Hz F 4 (+1)
	dw $69E ;  370.3 Hz F#4 (+1)
	dw $6B2 ;  392.4 Hz G 4 (+2)
	dw $6C4 ;  414.8 Hz G#4 (-2)
	dw $6D6 ;  439.8 Hz A 4 (-1)
	dw $6E7 ;  466.4 Hz A#4 (+1)
	dw $6F7 ;  494.6 Hz B 4 (+3)
	dw $706 ;  524.3 Hz C 5 (+3)
	dw $714 ;  555.4 Hz C#5 (+3)
	dw $721 ;  587.8 Hz D 5 (+1)
	dw $72D ;  621.2 Hz D#5 (-3)
	dw $739 ;  658.7 Hz E 5 (-2)
	dw $744 ;  697.2 Hz F 5 (-3)
	dw $74F ;  740.5 Hz F#5 (+1)
	dw $759 ;  784.9 Hz G 5 (+2)
	dw $762 ;  829.6 Hz G#5 (-2)
	dw $76B ;  879.7 Hz A 5 (-1)
	dw $773 ;  929.6 Hz A#5 (-5)
	dw $77B ;  985.5 Hz B 5 (-4)
	dw $783 ; 1048.6 Hz C 6 (+3)
	dw $78A ; 1110.8 Hz C#6 (+3)
	dw $790 ; 1170.3 Hz D 6 (-6)
	dw $797 ; 1248.3 Hz D#6 (+5)
	dw $79D ; 1324.0 Hz E 6 (+7)
	dw $7A2 ; 1394.4 Hz F 6 (-3)
	dw $7A7 ; 1472.7 Hz F#6 (-9)
	dw $7AC ; 1560.4 Hz G 6 (-8)
	dw $7B1 ; 1659.1 Hz G#6 (-2)
	dw $7B6 ; 1771.2 Hz A 6 (+11)
	dw $7BA ; 1872.5 Hz A#6 (+7)
	dw $7BE ; 1985.9 Hz B 6 (+9)
	dw $7C1 ; 2080.5 Hz C 7 (-10)
	dw $7C4 ; 2184.5 Hz C#7 (-26) Bug! 7C5 is closer
	dw $7C8 ; 2340.6 Hz D 7 (-6)
	dw $7CB ; 2473.1 Hz D#7 (-11)
	dw $7CE ; 2621.4 Hz E 7 (-10)
	dw $7D1 ; 2788.8 Hz F 7 (-3)
	dw $7D4 ; 2978.9 Hz F#7 (+11)
	dw $7D6 ; 3120.8 Hz G 7 (-8)
	dw $7D9 ; 3360.8 Hz G#7 (+20)
	dw $7DB ; 3542.5 Hz A 7 (+11)
	dw $7DD ; 3744.9 Hz A#7 (+7)
	dw $7DF ; 3971.9 Hz B 7 (+9)

Data_6F06:: ; 6F06
	db $00
	db $00, $00, $00, $00, $C0 ; 1
	db $A1 ,$00, $3A, $00, $C0 ; 6
	db $B1, $00, $29, $01, $C0 ; 11
	db $81, $00, $29, $04, $C0 ; 16

TriangeWavePattern:: ; 6F1B
	db $01, $23, $45, $67, $89, $AB, $CD, $EF
	db $FE, $DC, $BA, $98, $76, $54, $32, $10

SawtoothWavePattern:: ; 6F2B
	db $01, $12, $23, $34, $45, $56, $67, $78
	db $89, $9A, $AB, $BC, $CD, $DD, $EE, $FF

WavePattern_6F3B:: ; 6F3B ~sum of three cosines.
	db $01, $23, $56, $78, $99, $98, $76, $67
	db $9A, $DF, $FE, $C9, $85, $42, $11, $00

BossCryWavePattern:: ; 6F4B
	db $01, $23, $45, $67, $89, $AB, $CC, $CD
	db $00, $0C, $B0, $BB, $00, $FB, $BB, $BB

; Note lengths?
Data_6F5B:: ; 6F5B
	db 2, 3, 6, 12, 24, 48
	db       9, 18, 36
	db    4, 8

Data_6F66:: ; 6F66
	db 2, 4,  8, 16, 32, 64
	db       12, 24, 48
	db    5, 10
	db 1

Data_6F72:: ; 6F72
	db 0, 5, 10, 20, 40, 80
	db       15, 30, 60

Data_6F7B:: ; 6F7B
	db 3, 6, 12, 24, 48, 96
	db       18, 36, 72
	db    8, 16

Data_6F86:: ; 6F86
	db 0, 7, 14, 28, 56, 112
	db       21, 42, 84

; Unused, 8/7 times slower than the previous one
Data_6F8F:: ; 6F8F
	db 4, 8, 16, 32, 64, 128
	db       24, 48, 96

SECTION "TODO name", ROMX[$7FF0], BANK[3]
; gets called from timer interrupt
Call_7FF0:: ; 7FF0
	jp _Call_7FF0

InitSound:: ; 7FF3
	jp _InitSound
