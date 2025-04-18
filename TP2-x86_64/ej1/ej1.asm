; /** defines bool y puntero **/
%define NULL 0
%define TRUE 1
%define FALSE 0

section .data

section .text

global string_proc_list_create_asm
global string_proc_node_create_asm
global string_proc_list_add_node_asm
global string_proc_list_concat_asm

; FUNCIONES auxiliares que pueden llegar a necesitar:
extern malloc
extern free
extern str_concat

string_proc_list_create_asm:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     edi, 16
        call    malloc
        mov     QWORD  [rbp-8], rax
        cmp     QWORD  [rbp-8], 0
        jne     .init_success
        mov     eax, 0
        jmp     .return
.init_success:
        mov     rax, QWORD  [rbp-8]
        mov     QWORD  [rax], 0
        mov     rax, QWORD  [rbp-8]
        mov     QWORD  [rax+8], 0
        mov     rax, QWORD  [rbp-8]
.return:
        leave
        ret

string_proc_node_create_asm:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 32
        mov     eax, edi
        mov     QWORD  [rbp-32], rsi
        mov     BYTE  [rbp-20], al
        mov     edi, 32
        call    malloc
        mov     QWORD  [rbp-8], rax
        cmp     QWORD  [rbp-8], 0
        je      .return_null
        cmp     QWORD  [rbp-32], 0
        jne     .init_node_fields
.return_null:
        mov     eax, 0
        jmp     .return_node
.init_node_fields:
        mov     rax, QWORD  [rbp-8]
        movzx   edx, BYTE  [rbp-20]
        mov     BYTE  [rax+16], dl
        mov     rax, QWORD  [rbp-8]
        mov     rdx, QWORD  [rbp-32]
        mov     QWORD  [rax+24], rdx
        mov     rax, QWORD  [rbp-8]
        mov     QWORD  [rax], 0
        mov     rax, QWORD  [rbp-8]
        mov     QWORD  [rax+8], 0
        mov     rax, QWORD  [rbp-8]
.return_node:
        leave
        ret

string_proc_list_add_node_asm:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 48
        mov     QWORD  [rbp-24], rdi
        mov     eax, esi
        mov     QWORD  [rbp-40], rdx
        mov     BYTE  [rbp-28], al
        cmp     QWORD  [rbp-24], 0
        je      .invalid_input
        cmp     QWORD  [rbp-40], 0
        je      .invalid_input
        movzx   eax, BYTE  [rbp-28]
        mov     rdx, QWORD  [rbp-40]
        mov     rsi, rdx
        mov     edi, eax
        call    string_proc_node_create_asm
        mov     QWORD  [rbp-8], rax
        mov     rax, QWORD  [rbp-24]
        mov     rax, QWORD  [rax]
        test    rax, rax
        jne     .append_to_list
        mov     rax, QWORD  [rbp-24]
        mov     rdx, QWORD  [rbp-8]
        mov     QWORD  [rax], rdx
        mov     rax, QWORD  [rbp-24]
        mov     rdx, QWORD  [rbp-8]
        mov     QWORD  [rax+8], rdx
        jmp     .return_void
.append_to_list:
        mov     rax, QWORD  [rbp-24]
        mov     rax, QWORD  [rax+8]
        mov     rdx, QWORD  [rbp-8]
        mov     QWORD  [rax], rdx
        mov     rax, QWORD  [rbp-24]
        mov     rdx, QWORD  [rax+8]
        mov     rax, QWORD  [rbp-8]
        mov     QWORD  [rax+8], rdx
        mov     rax, QWORD  [rbp-24]
        mov     rdx, QWORD  [rbp-8]
        mov     QWORD  [rax+8], rdx
        jmp     .return_void
.invalid_input:
        nop
.return_void:
        leave
        ret

string_proc_list_concat_asm:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 64
        mov     QWORD  [rbp-40], rdi
        mov     eax, esi
        mov     QWORD  [rbp-56], rdx
        mov     BYTE  [rbp-44], al
        cmp     QWORD  [rbp-40], 0
        je      .invalid_input
        cmp     QWORD  [rbp-56], 0
        jne     .concat_first_hash
.invalid_input:
        mov     eax, 0
        jmp     .return_result
.concat_first_hash:
        mov     rax, QWORD  [rbp-56]
        mov     rsi, rax
        mov     edi, LC0
        call    str_concat_asm
        mov     QWORD  [rbp-8], rax
        cmp     QWORD  [rbp-8], 0
        jne     .start_loop
        mov     eax, 0
        jmp     .return_result
.start_loop:
        mov     rax, QWORD  [rbp-40]
        mov     rax, QWORD  [rax]
        mov     QWORD  [rbp-16], rax
        jmp     .loop_check_node
.loop_node:
        mov     rax, QWORD  [rbp-16]
        movzx   eax, BYTE  [rax+16]
        cmp     BYTE  [rbp-44], al
        jne     .skip_node
        mov     rax, QWORD  [rbp-8]
        mov     QWORD  [rbp-24], rax
        mov     rax, QWORD  [rbp-16]
        mov     rdx, QWORD  [rax+24]
        mov     rax, QWORD  [rbp-8]
        mov     rsi, rdx
        mov     rdi, rax
        call    str_concat_asm
        mov     QWORD  [rbp-8], rax
        cmp     QWORD  [rbp-8], 0
        jne     .free_old_hash
        mov     rax, QWORD  [rbp-24]
        mov     rdi, rax
        call    free
        mov     eax, 0
        jmp     .return_result
.free_old_hash:
        mov     rax, QWORD  [rbp-24]
        mov     rdi, rax
        call    free
.skip_node:
        mov     rax, QWORD  [rbp-16]
        mov     rax, QWORD  [rax]
        mov     QWORD  [rbp-16], rax
.loop_check_node:
        cmp     QWORD  [rbp-16], 0
        jne     .loop_node
        mov     rax, QWORD  [rbp-8]
.return_result:
        leave
        ret