SECTION "Entry point", ROM0[$0100]
	nop
	jp Start

SECTION "Start", ROM0[$0150]
Start::	; 0150
	jp Init