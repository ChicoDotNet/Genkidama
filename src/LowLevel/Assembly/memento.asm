; Memento pattern — Linux x86-64 / NASM.
; The originator owns capture and restore. The caretaker keeps a separate byte snapshot.
; Exit 1 on any broken invariant; otherwise print the canonical success marker.

section .data
    document_state db 0        ; 0=draft, 1=published, 2=local-edit
    snapshot_state db 255      ; caretaker-owned snapshot storage
    success db "Assembly Memento: passed", 10
    success_len equ $ - success

section .text
    global _start

save_memento:
    mov al, [document_state]
    mov [snapshot_state], al
    ret

publish_document:
    mov byte [document_state], 1
    ret

restore_memento:
    mov al, [snapshot_state]
    mov [document_state], al
    ret

fail:
    mov rax, 60
    mov rdi, 1
    syscall

_start:
    call save_memento
    cmp byte [snapshot_state], 0
    jne fail

    call publish_document
    cmp byte [document_state], 1
    jne fail
    cmp byte [snapshot_state], 0
    jne fail

    call restore_memento
    cmp byte [document_state], 0
    jne fail

    ; Mutating the restored originator must not mutate the caretaker snapshot.
    mov byte [document_state], 2
    cmp byte [snapshot_state], 0
    jne fail

    mov rax, 1
    mov rdi, 1
    mov rsi, success
    mov rdx, success_len
    syscall

    mov rax, 60
    xor rdi, rdi
    syscall
