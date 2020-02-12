; Copy BC bytes from HL to DE
CopyData::	; 05DE
	ld a, [hli]
	ld [de], a
	inc de
	dec bc
	ld a, b
	or c
	jr nz, CopyData
	ret