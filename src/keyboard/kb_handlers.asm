;KBHandleCharacter
;KBHandleArrowKey
;KBHandleTab
;KBHandleEnter
;KBHandleBackspace
;KBHandleDelete
;KBHandleFunctionKey
;KBHandleEscape

KBHandleCharacter::;a = character in ASCII
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_TYPING
  ret nz

  ld a, b;ASCII

  ret

KBHandleArrowKey::;a = DPad key
  
  ret

KBHandleTab::
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_TYPING
  ret nz


  ret

KBHandleEnter::
  ld a, [kb_mode]
  cp a, KB_MODE_BUTTONS
  ret nz
  
  ret

KBHandleBackspace::
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_TYPING
  ret nz

  ret

KBHandleDelete::
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_TYPING
  ret nz

  ret

KBHandleFunctionKey::;a = function key

  ret

KBHandleEscape::
  ld b, a
  ld a, [kb_mode]
  cp a, KB_MODE_BUTTONS
  ret nz
  
  ret