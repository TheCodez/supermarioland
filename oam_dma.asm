DMARoutine::
	ld a, HIGH(wOAMBuffer)
	ldh [rDMA], a
	ld a, $28
.wait
	dec a
	jr nz, .wait
	ret
DMARoutineEnd: