#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdint.h>

//importar NEXT_STATE y CURRENT_STATE
#include "shell.h"
#include "math.h"  

// Máscaras para diferentes longitudes de opcodes
#define OPCODE_MASK_IMM 0b11111111110000000000000000000000 // Máscara para Immediate opcodes con bits del 31 al 22 prendidos
#define OPCODE_MASK_EXT_REG 0b11111111111000000000000000000000 // Máscara para Extended Register opcodes con bits del 31 al 21 prendidos
#define OPCODE_MASK_SHIFT_REG 0b11111111000000000000000000000000 // Máscara para Shifted Register opcodes con bits del 31 al 24 prendidos
#define OPCODE_MASK_B 0b11111100000000000000000000000000 //Máscara para B opcode con bits del 31 al 26 prendidos
#define OPCODE_MASK_BR 0b11111111111111111111110000000000 //Máscara para BR con bits del 31 al 10 prendidos

// Códigos de opcodes
#define OPCODE_ADDS_IMM 0b10110001000000000000000000000000 // Opcode de ADDS Immediate
#define OPCODE_ADDS_EXT_REG 0b10101011000000000000000000000000 // Opcode de ADDS Extended Register
#define OPCODE_SUBS_IMM 0b11110001000000000000000000000000 // Opcode de SUBS Immediate
#define OPCODE_SUBS_EXT_REG 0b11101011000000000000000000000000 // Opcode de SUBS Extended Register
#define OPCODE_HLT 0b11010100010000000000000000000000 // Opcode de HLT
#define OPCODE_CMP_IMM 0b11110001000000000000000000000000 // Opcode de CMP Immediate
#define OPCODE_CMP_EXT_REG 0b11101011001000000000000000000000 // Opcode de CMP Extended Register
#define OPCODE_ANDS_SHIFTED_REG 0b11101010000000000000000000000000 // Opcode de ANDS Shifted Register
#define OPCODE_EOR_SHIFTED_REG 0b11001010000000000000000000000000 // Opcode de EOR Shifted Register
#define OPCODE_ORR_SHIFTED_REG 0b10101010000000000000000000000000 // Opcode de ORR cShifted Register
#define OPCODE_B 0b00010100000000000000000000000000 // Opcode de B
#define OPCODE_BR 0b11010110000111110000000000000000 // Opcode de BR
#define OPCODE_B_COND 0b01010100000000000000000000000000 // Opcode de B.Cond
#define OPCODE_LSL_IMM 0b11010011010000000000000000000000 // Opcode de LSL Immediate
#define OPCODE_LSR_IMM 0b11010011010000000000000000000000 // Opcode de LSR Immediate
#define OPCODE_STUR 0b11111000000000000000000000000000 // Opcode de STUR
#define OPCODE_STURB 0b00111000000000000000000000000000 // Opcode de STURB
#define OPCODE_STURH 0b01111000000000000000000000000000 // Opcode de STURH
#define OPCODE_LDUR 0b11111000010000000000000000000000 // Opcode de LDUR
#define OPCODE_LDURH 0b01111000010000000000000000000000 // Opcode de LDURH
#define OPCODE_LDURB 0b00111000010000000000000000000000 // Opcode de LDURB
#define OPCODE_MOVZ 0b11010010100000000000000000000000 // Opcode de MOVZ
#define OPCODE_ADD_IMM 0b10010001000000000000000000000000 // Opcode de ADD Immediate
#define OPCODE_ADD_EXT_REG 0b10001011001000000000000000000000 // Opcode de Add Extended Register
#define OPCODE_MUL 0b10011011000000000000000000000000 // Opcode de MUL
#define OPCODE_CBZ 0b10110100000000000000000000000000 // Opcode de CBZ
#define OPCODE_CBNZ 0b10110101000000000000000000000000 // Opcode de CBNZ

#define MEMORY_START_ADDRESS 0x10000000
#define MEMORY_SIZE 0x00100000

// Función para leer una instrucción de memoria
uint32_t fetch(uint64_t pc) {
    return mem_read_32(pc);
}

uint64_t mem_read_64(uint64_t address)
{
    uint32_t lower_bits = mem_read_32(address);
    uint32_t upper_bits = mem_read_32(address + 4);
    
    uint64_t result = ((uint64_t)upper_bits << 32) | lower_bits;
    
    return result;
}

// Función para decodificar e interpretar una instrucción
void decode_instruction(uint32_t instruction);

// Funciones para ejecutar las instrucciones implementadas
void ADDS_IMMEDIATE(uint32_t instruction);
void ADDS_EXTENDED_REGISTER(uint32_t instruction);
void SUBS_IMMEDIATE(uint32_t instruction);
void SUBS_EXTENDED_REGISTER(uint32_t instruction);
void HLT_EXECUTE(uint32_t instruction);
void CMP_IMMEDIATE(uint32_t instruction);
void CMP_EXTENDED_REGISTER(uint32_t instruction);
void ANDS_SHIFTED_REGISTER(uint32_t instruction);
void EOR_SHIFTED_REGISTER(uint32_t instruction);
void ORR_SHIFTED_REGISTER(uint32_t instruction);
void B(uint32_t instruction);
void BR(uint32_t instruction);
void Bcond(uint32_t instruction);
void BEQ_EXECUTE(uint32_t instruction);
void BNE_EXECUTE(uint32_t instruction);
void BGT_EXECUTE(uint32_t instruction);
void BLT_EXECUTE(uint32_t instruction);
void BGE_EXECUTE(uint32_t instruction);
void BLE_EXECUTE(uint32_t instruction);
void LSL_IMMEDIATE(uint32_t instruction);
void LSR_IMMEDIATE(uint32_t instruction);
void STUR(unsigned int instruction);
void STURB(unsigned int instruction);
void STURH(unsigned int instruction);
void LDUR(unsigned int instruction);
void LDURH(unsigned int instruction);
void LDURB(unsigned int instruction);
void MOVZ(uint32_t instruction);
void ADD_IMMEDIATE(uint32_t instruction);
void ADD_EXTENDED_REGISTER(uint32_t instruction);
void MUL(uint32_t instruction);
void CBZ(uint32_t instruction);
void CBNZ(uint32_t instruction);

void decode_instruction(uint32_t instruction) {

    if ((instruction & OPCODE_MASK_IMM) == (OPCODE_ADDS_IMM)) {
        // printf("Instruction: ADDS_IMMEDIATE\n");
        ADDS_IMMEDIATE(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == (OPCODE_ADDS_EXT_REG)) {
        // printf("Instruction: ADDS_EXTENDED_REGISTER\n");
        ADDS_EXTENDED_REGISTER(instruction);
    } else if ((instruction & OPCODE_MASK_IMM) == (OPCODE_SUBS_IMM)) {
        // printf("Instruction: SUBS_IMMEDIATE\n");
        SUBS_IMMEDIATE(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == (OPCODE_SUBS_EXT_REG)) {
        // printf("Instruction: SUBS_EXTENDED_REGISTER\n");
        SUBS_EXTENDED_REGISTER(instruction);
    } else if (instruction == OPCODE_HLT) {
        // printf("Instruction: HLT_EXECUTE\n");
        HLT_EXECUTE(instruction);
    } else if ((instruction & OPCODE_MASK_IMM) == OPCODE_CMP_IMM) {
        CMP_IMMEDIATE(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == OPCODE_CMP_EXT_REG) {
        CMP_EXTENDED_REGISTER(instruction);
    } else if ((instruction & OPCODE_MASK_SHIFT_REG) == (OPCODE_ANDS_SHIFTED_REG)){
        // printf("Instruction: ANDS_SHIFTED_REGISTER\n");
        ANDS_SHIFTED_REGISTER(instruction);
    } else if ((instruction & OPCODE_MASK_SHIFT_REG) == (OPCODE_EOR_SHIFTED_REG)){
        // printf("Instruction: EOR_SHIFTED_REGISTER\n");
        EOR_SHIFTED_REGISTER(instruction);
    } else if ((instruction & OPCODE_MASK_SHIFT_REG) == (OPCODE_ORR_SHIFTED_REG)){
        // printf("Instruction: ORR_SHIFTED_REGISTER\n");
        ORR_SHIFTED_REGISTER(instruction);
    } else if ((instruction & OPCODE_MASK_B) == (OPCODE_B)){
        // printf("Instruction: B\n");
        B(instruction);
    } else if ((instruction & OPCODE_MASK_BR) == (OPCODE_BR)) {
        // printf("Instruction: BR\n");
        BR(instruction);
    } else if ((instruction & OPCODE_MASK_SHIFT_REG) == (OPCODE_B_COND)){
        // printf("Instruction: Bcond\n");
        Bcond(instruction);
    } else if ((instruction & OPCODE_MASK_IMM) == (OPCODE_LSL_IMM)) {
        // printf("Instruction: LSL_IMMEDIATE\n");
        uint32_t imms = (instruction >> 10) & 0x3F;
        if (imms != 63) {
            LSL_IMMEDIATE(instruction);
        } 
    } else if ((instruction & OPCODE_MASK_IMM) == (OPCODE_LSR_IMM)) {
        // printf("Instruction: LSR_IMMEDIATE\n");
        uint32_t imms = (instruction >> 10) & 0x3F;
        if (imms == 63) {
            LSR_IMMEDIATE(instruction);
        }
    } else if ((instruction & OPCODE_MASK_EXT_REG) == OPCODE_STUR) {
        // printf("Instruction: STUR\n");
        STUR(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == OPCODE_STURB) {
        // printf("Instruction: STURB\n");
        STURB(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == OPCODE_STURH) {
        // printf("Instruction: STURH\n");
        STURH(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == OPCODE_LDUR) {
        // printf("Instruction: LDUR\n");
        LDUR(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == OPCODE_LDURH) {
        // printf("Instruction: LDURH\n");
        LDURH(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == OPCODE_LDURB) {
        // printf("Instruction: LDURB\n");
        LDURB(instruction);
    } else if ((instruction & OPCODE_MASK_IMM) == OPCODE_MOVZ){
        // printf("Instruction: MOVZ\n");
        MOVZ(instruction);
    } else if ((instruction & OPCODE_MASK_IMM) == OPCODE_ADD_IMM) {
        ADD_IMMEDIATE(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == OPCODE_ADD_EXT_REG) {
        ADD_EXTENDED_REGISTER(instruction);
    } else if ((instruction & OPCODE_MASK_EXT_REG) == OPCODE_MUL) {
        MUL(instruction);
    } else if ((instruction & OPCODE_MASK_SHIFT_REG) == OPCODE_CBZ) {
        CBZ(instruction);
    } else if ((instruction & OPCODE_MASK_SHIFT_REG) == OPCODE_CBNZ) {
        CBNZ(instruction);
    } else {
        printf("Instrucción no reconocida\n");
    }
}

void ADDS_IMMEDIATE(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rn = (instruction >> 5) & 0x1F; // Primer operando
    uint32_t Rd = (instruction >> 0) & 0x1F; // Registro destino
    uint32_t imm = (instruction >> 10) & 0xFFF; // Immediate de 12 bits
    uint32_t shift = (instruction >> 22) & 0x3; // Campo de desplazamiento

    // Ajustar el Immediate según el campo de desplazamiento
    if (shift == 0b01) {
        imm <<= 12; // Desplazar 12 bits hacia la izquierda
    }

    // Leer operandos
    uint64_t Rn_val = NEXT_STATE.REGS[Rn];
    
    // Realizar operación
    uint64_t result = Rn_val + imm;
    
    // Escribir resultado en el registro destino
    NEXT_STATE.REGS[Rd] = result;
    
    // Actualizar flags
    NEXT_STATE.FLAG_N = (result >> 31) & 0x1; // Bit más significativo para N
    NEXT_STATE.FLAG_Z = (result == 0) ? 1 : 0;
    
    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void ADDS_EXTENDED_REGISTER(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rn = (instruction >> 5) & 0x1F; // Primer operando
    uint32_t Rm = (instruction >> 16) & 0x1F; // Segundo operando
    uint32_t Rd = (instruction >> 0) & 0x1F; // Registro destino
    
    // Leer operandos
    uint64_t Rn_val = NEXT_STATE.REGS[Rn];
    uint64_t Rm_val = NEXT_STATE.REGS[Rm];
    
    // Realizar operación
    uint64_t result = Rn_val + Rm_val;
    
    // Escribir resultado en el registro destino
    NEXT_STATE.REGS[Rd] = result;
    
    // Actualizar flags
    NEXT_STATE.FLAG_N = (result >> 31) & 0x1; // Bit más significativo para N
    NEXT_STATE.FLAG_Z = (result == 0) ? 1 : 0;
    
    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void SUBS_IMMEDIATE(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rn = (instruction >> 5) & 0x1F; // Primer operando
    uint32_t Rd = (instruction >> 0) & 0x1F; // Registro destino
    uint32_t imm = (instruction >> 10) & 0xFFF; // Immediate de 12 bits
    uint32_t shift = (instruction >> 22) & 0x3; // Campo de desplazamiento

    // Ajustar el Immediate según el campo de desplazamiento
    if (shift == 0b01) {
        imm <<= 12; // Desplazar 12 bits hacia la izquierda
    } else {
        imm = imm & 0xFFFFFFFFFFFFF; // Extensión cero para 64 bits
    }

    // Leer operandos
    uint64_t Rn_val = NEXT_STATE.REGS[Rn];
    
    // Realizar operación
    uint64_t result = Rn_val - imm;
    
    // Escribir el resultado en el registro destino si no es el registro cero (XZR)
    if (Rd != 31) {
        NEXT_STATE.REGS[Rd] = result & 0xFFFFFFFFFFFFFFFF; // Guardar solo los 32 bits inferiores
    }

    // Actualizar las flags
    NEXT_STATE.FLAG_N = (result >> 31) & 0x1; // Bit más significativo para N
    NEXT_STATE.FLAG_Z = (result == 0) ? 1 : 0;
    
    // Actualizar el program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void SUBS_EXTENDED_REGISTER(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rn = (instruction >> 5) & 0x1F; // Primer operando
    uint32_t Rd = (instruction >> 0) & 0x1F; // Regsitro destino
    uint32_t Rm = (instruction >> 16) & 0x1F; // Segundo operando
    
    // Leer operandos
    uint64_t Rn_val = NEXT_STATE.REGS[Rn];
    uint64_t Rm_val = NEXT_STATE.REGS[Rm];
    
    // Realizar operación
    uint64_t result = Rn_val - Rm_val;
    
    // Escribir resultado en el registro destino si no es el registro cero (XZR)
    if (Rd != 31) {
        NEXT_STATE.REGS[Rd] = result;
    }

    // Actualizar flags
    NEXT_STATE.FLAG_N = (result >> 31) & 0x1; // Bit más significativo para N
    NEXT_STATE.FLAG_Z = (result == 0) ? 1 : 0;
    
    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void HLT_EXECUTE(uint32_t instruction) {
    // Detener la ejecución
    RUN_BIT = 0;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void CMP_IMMEDIATE(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t imm = (instruction >> 10) & 0xFFF; // Valor inmediato
    uint32_t Rn = (instruction >> 5) & 0x1F; // Primer operando

    // Realizar la operación CMP y descartar el resultado, solo actualizar flags
    uint64_t result = NEXT_STATE.REGS[Rn] - imm;

    // Actualizar las banderas según el resultado
    NEXT_STATE.FLAG_N = (result >> 63) & 0x1; // Bit más significativo para N (en arquitectura de 64 bits)
    NEXT_STATE.FLAG_Z = (result == 0) ? 1 : 0;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void CMP_EXTENDED_REGISTER(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rm = (instruction >> 16) & 0x1F; // Segundo operando
    uint32_t Rn = (instruction >> 5) & 0x1F; // Primer operando

    // Realizar la operación CMP y descartar el resultado, solo actualizar flags
    uint64_t result = NEXT_STATE.REGS[Rn] - NEXT_STATE.REGS[Rm];

    // Actualizar las banderas según el resultado
    NEXT_STATE.FLAG_N = (result >> 63) & 0x1; // Bit más significativo para N (en arquitectura de 64 bits)
    NEXT_STATE.FLAG_Z = (result == 0) ? 1 : 0;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void ANDS_SHIFTED_REGISTER(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rd = (instruction >> 0) & 0x1F;  // Registro destino
    uint32_t Rn = (instruction >> 5) & 0x1F;  // Primer operando
    uint32_t Rm = (instruction >> 16) & 0x1F; // Segundo operando (registro para el desplazamiento)
    //Asumo que shift es 0

    // Leer valores de los registros
    uint32_t Rn_val = NEXT_STATE.REGS[Rn];
    uint32_t Rm_val = NEXT_STATE.REGS[Rm];

    // Realizar la operación AND y almacenar el resultado en el registro destino
    uint32_t result = Rn_val & Rm_val;
    NEXT_STATE.REGS[Rd] = result;

    // Actualizar las banderas según el resultado
    NEXT_STATE.FLAG_N = (result >> 31) & 0x1; // Bit más significativo para N
    NEXT_STATE.FLAG_Z = (result == 0) ? 1 : 0;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void EOR_SHIFTED_REGISTER(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rd = (instruction >> 0) & 0x1F;  // Registro destino
    uint32_t Rn = (instruction >> 5) & 0x1F;  // Primer operando
    uint32_t Rm = (instruction >> 16) & 0x1F; // Segundo operando (registro para el desplazamiento)
    //Asumo que shift es 0

    // Leer valores de los registros
    uint32_t Rn_val = NEXT_STATE.REGS[Rn];
    uint32_t Rm_val = NEXT_STATE.REGS[Rm];

    // Realizar la operación EOR y almacenar el resultado en el registro destino
    uint32_t result = Rn_val ^ Rm_val;
    NEXT_STATE.REGS[Rd] = result;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void ORR_SHIFTED_REGISTER(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rd = (instruction >> 0) & 0x1F;  // Registro destino
    uint32_t Rn = (instruction >> 5) & 0x1F;  // Primer operando
    uint32_t Rm = (instruction >> 16) & 0x1F; // Segundo operando (registro para el desplazamiento)
    //Asumo que shift es 0

    // Leer valores de los registros
    uint32_t Rn_val = NEXT_STATE.REGS[Rn];
    uint32_t Rm_val = NEXT_STATE.REGS[Rm];

    // Realizar la operación ORR y almacenar el resultado en el registro destino
    uint32_t result = Rn_val | Rm_val;
    NEXT_STATE.REGS[Rd] = result;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void B(uint32_t instruction) {
    // Extraer el offset Immediate de la instrucción (26 bits)
    uint32_t imm26 = instruction & 0x3FFFFFF;

    // Sign extend para obtener un valor de 32 bits
    int32_t offset = imm26;
    if (offset & 0x2000000) { // Revisar el bit de signo en el bit 25
        offset |= 0xFC000000; // Extender el signo si el bit de signo está encendido
    }

    // Calcular la dirección de salto sumando el offset al program counter actual
    uint32_t jump_address = CURRENT_STATE.PC + (offset*4);

    // Actualizar el program counter con la dirección de salto
    NEXT_STATE.PC = jump_address;
}

void BR(uint32_t instruction) {
    // Extraer el registro de destino de la instrucción
    uint32_t Rn = (instruction >> 5) & 0x1F;

    // Calcular la dirección de salto
    uint32_t jump_address = CURRENT_STATE.REGS[Rn];

    // Actualizar el program counter con la dirección de salto
    NEXT_STATE.PC = jump_address;
}

void Bcond(uint32_t instruction) {
    // Determinar la condición de salto
    uint32_t condition = instruction & 0xF;

    // Verificar la condición y realizar el salto si es verdadera
    if(condition == 0b0000){
        // BEQ
        BEQ_EXECUTE(instruction);
    } else if(condition == 0b0001){
        // BNE
        BNE_EXECUTE(instruction);
    } else if(condition == 0b1100){
        // BGT
        BGE_EXECUTE(instruction);
    } else if(condition == 0b1011){
        // BLT
        BLT_EXECUTE(instruction);
    } else if(condition == 0b1010){
        // BGE
        BGE_EXECUTE(instruction);
    } else if(condition == 0b1101){
        // BLE
        BLE_EXECUTE(instruction);
    } else {
        // Condición no reconocida, no se realiza el salto
        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
    }
}

void BEQ_EXECUTE(uint32_t instruction) {

    // Realizar el salto si los registros son iguales (si el flag Z está activado)
    if (CURRENT_STATE.FLAG_Z == 1) {
        // Extraer el offset Immediate de la instrucción (19 bits)
        uint32_t imm19 = (instruction >> 5) & 0x7FFFF; // Extracción de los bits 23-5

        // Sign extend para obtener un valor de 32 bits
        int32_t offset = imm19;
        if (offset & 0x40000) { // Revisar el bit de signo en el bit 18
            offset |= 0xFFFC0000; // Extender el signo si el bit de signo está encendido
        }

        // Calcular la dirección de salto sumando el offset al program counter actual
        uint32_t jump_address = CURRENT_STATE.PC + (offset*4);

        // Actualizar el program counter con la dirección de salto
        NEXT_STATE.PC = jump_address;
    
    } else {
        NEXT_STATE.PC = CURRENT_STATE.PC + 4; // Continuar con la siguiente instrucción
    }
}

void BNE_EXECUTE(uint32_t instruction) {
    // Realizar el salto si los registros son diferentes (si el flag Z está desactivado)
    if (CURRENT_STATE.FLAG_Z == 0) {
        // Extraer el offset Immediate de la instrucción (19 bits)
        uint32_t imm19 = (instruction >> 5) & 0x7FFFF; // Extracción de los bits 23-5

        // Sign extend para obtener un valor de 32 bits
        int32_t offset = imm19;
        if (offset & 0x40000) { // Revisar el bit de signo en el bit 18
            offset |= 0xFFFC0000; // Extender el signo si el bit de signo está encendido
        }

        // Calcular la dirección de salto sumando el offset al program counter actual
        uint32_t jump_address = CURRENT_STATE.PC + (offset*4);

        // Actualizar el program counter con la dirección de salto
        NEXT_STATE.PC = jump_address;
    } else {
        NEXT_STATE.PC = CURRENT_STATE.PC + 4; // Continuar con la siguiente instrucción
    }

}

void BGT_EXECUTE(uint32_t instruction) {
    if (CURRENT_STATE.FLAG_N == 0 && CURRENT_STATE.FLAG_Z == 0) {
        // Extraer el offset Immediate de la instrucción (19 bits)
        uint32_t imm19 = (instruction >> 5) & 0x7FFFF; // Extracción de los bits 23-5

        // Sign extend para obtener un valor de 32 bits
        int32_t offset = imm19;
        if (offset & 0x40000) { // Revisar el bit de signo en el bit 18
            offset |= 0xFFFC0000; // Extender el signo si el bit de signo está encendido
        }

        // Calcular la dirección de salto sumando el offset al program counter actual
        uint32_t jump_address = CURRENT_STATE.PC + (offset*4);

        // Actualizar el program counter con la dirección de salto
        NEXT_STATE.PC = jump_address;
    } else {
        NEXT_STATE.PC = CURRENT_STATE.PC + 4; // Continuar con la siguiente instrucción
    }
}

void BLT_EXECUTE(uint32_t instruction) {
    if (CURRENT_STATE.FLAG_N != 0) {
        // Extraer el offset Immediate de la instrucción (19 bits)
        uint32_t imm19 = (instruction >> 5) & 0x7FFFF; // Extracción de los bits 23-5

        // Sign extend para obtener un valor de 32 bits
        int32_t offset = imm19;
        if (offset & 0x40000) { // Revisar el bit de signo en el bit 18
            offset |= 0xFFFC0000; // Extender el signo si el bit de signo está encendido
        }

        // Calcular la dirección de salto sumando el offset al program counter actual
        uint32_t jump_address = CURRENT_STATE.PC + (offset*4);

        // Actualizar el program counter con la dirección de salto
        NEXT_STATE.PC = jump_address;
    } else {
        NEXT_STATE.PC = CURRENT_STATE.PC + 4; // Continuar con la siguiente instrucción
    }
    
}

void BGE_EXECUTE(uint32_t instruction) {
    if(CURRENT_STATE.FLAG_N == 0) {
        // Extraer el offset Immediate de la instrucción (19 bits)
        uint32_t imm19 = (instruction >> 5) & 0x7FFFF; // Extracción de los bits 23-5

        // Sign extend para obtener un valor de 32 bits
        int32_t offset = imm19;
        if (offset & 0x40000) { // Revisar el bit de signo en el bit 18
            offset |= 0xFFFC0000; // Extender el signo si el bit de signo está encendido
        }

        // Calcular la dirección de salto sumando el offset al program counter actual
        uint32_t jump_address = CURRENT_STATE.PC + (offset*4);

        // Actualizar el program counter con la dirección de salto
        NEXT_STATE.PC = jump_address;
    } else {
        NEXT_STATE.PC = CURRENT_STATE.PC + 4; // Continuar con la siguiente instrucción
    }
    
}

void BLE_EXECUTE(uint32_t instruction) {
    if(!(CURRENT_STATE.FLAG_Z ==0 && CURRENT_STATE.FLAG_N == 0)) {
        // Extraer el offset Immediate de la instrucción (19 bits)
        uint32_t imm19 = (instruction >> 5) & 0x7FFFF; // Extracción de los bits 23-5

        // Sign extend para obtener un valor de 32 bits
        int32_t offset = imm19;
        if (offset & 0x40000) { // Revisar el bit de signo en el bit 18
            offset |= 0xFFFC0000; // Extender el signo si el bit de signo está encendido
        }

        // Calcular la dirección de salto sumando el offset al program counter actual
        uint32_t jump_address = CURRENT_STATE.PC + (offset*4);

        // Actualizar el program counter con la dirección de salto
        NEXT_STATE.PC = jump_address;
    } else {
        NEXT_STATE.PC = CURRENT_STATE.PC + 4; // Continuar con la siguiente instrucción
    }
   
}

void LSL_IMMEDIATE(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rd = (instruction >> 0) & 0x1F;  // Registro destino
    uint32_t Rn = (instruction >> 5) & 0x1F;  // Primer operando
    uint32_t imms = (instruction >> 10) & 0x3F; // Immediate de 6 bits
    uint32_t immr = (instruction >> 16) & 0x3F; // Immediate de 6 bits
    
        // Leer valores de los registros
        uint32_t Rn_val = NEXT_STATE.REGS[Rn];

        // Realizar la operación LSL y almacenar el resultado en el registro destino
        uint32_t result = Rn_val << (64-immr);
        NEXT_STATE.REGS[Rd] = result;

        // Actualizar las banderas según el resultado
        NEXT_STATE.FLAG_N = (result >> 31) & 0x1; // Bit más significativo para N
        NEXT_STATE.FLAG_Z = (result == 0) ? 1 : 0;      

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void LSR_IMMEDIATE(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rd = (instruction >> 0) & 0x1F;  // Registro destino
    uint32_t Rn = (instruction >> 5) & 0x1F;  // Primer operando
    uint32_t imms = (instruction >> 10) & 0x3F; // Immediate de 6 bits
    uint32_t immr = (instruction >> 16) & 0x3F; // Immediate de 6 bits

        // Leer valores de los registros
        uint32_t Rn_val = NEXT_STATE.REGS[Rn];

        // Realizar la operación LSR y almacenar el resultado en el registro destino
        uint32_t result = Rn_val >> immr;
        NEXT_STATE.REGS[Rd] = result;

        // Actualizar las banderas según el resultado
        NEXT_STATE.FLAG_N = (result >> 31) & 0x1; // Bit más significativo para N
        NEXT_STATE.FLAG_Z = (result == 0) ? 1 : 0;

    // Actualizar program counter
    NEXT_STATE.PC += 4;
}

void STUR(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rt = (instruction >> 0) & 0x1F;    // Registro fuente
    uint32_t Rn = (instruction >> 5) & 0x1F;    // Registro base
    uint32_t imm9 = (instruction >> 12) & 0x1FF; // Immediate de 9 bits
    int32_t offset = imm9;  // Sign extend para obtener un valor de 32 bits

    // Verificar si el bit más significativo del offset está encendido
    if (offset & 0x100) {
        offset |= 0xFFFFFE00; // Extender el signo
    }

    // Calcular la dirección de memoria de destino
    uint32_t address = NEXT_STATE.REGS[Rn] + offset;

    // Verificar si la dirección de memoria está dentro del rango permitido
    if (address < MEMORY_START_ADDRESS || address >= MEMORY_SIZE + MEMORY_START_ADDRESS) {
        // La dirección de memoria está fuera del rango permitido
        printf("Error: Dirección de memoria fuera de rango permitido.\n");
        return;
    }

    // Escribir el valor del registro fuente en la memoria
    mem_write_32(address, NEXT_STATE.REGS[Rt]);

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void STURB(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rt = (instruction >> 0) & 0x1F;    // Registro fuente
    uint32_t Rn = (instruction >> 5) & 0x1F;    // Registro base
    uint32_t imm9 = (instruction >> 12) & 0x1FF; // Immediate de 9 bits
    int32_t offset = imm9;  // Sign extend para obtener un valor de 32 bits

    // Verificar si el bit más significativo del offset está encendido
    if (offset & 0x100) {
        offset |= 0xFFFFFE00; // Extender el signo
    }

    // Calcular la dirección de memoria de destino
    uint32_t address = NEXT_STATE.REGS[Rn] + offset;

    // Verificar si la dirección de memoria está dentro del rango permitido
    if (address < MEMORY_START_ADDRESS || address >= MEMORY_SIZE + MEMORY_START_ADDRESS) {
        // La dirección de memoria está fuera del rango permitido
        printf("Error: Dirección de memoria fuera de rango permitido.\n");
        return;
    }
    
    //recorto los primeros 8 bits que vamos a guardar
    uint32_t byte_value = NEXT_STATE.REGS[Rt] & 0xFF;

    // Escribir los primeros 8 bits de memoria en la dirección especificada
    mem_write_32(address, byte_value);

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void STURH(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rt = (instruction >> 0) & 0x1F;    // Registro fuente
    uint32_t Rn = (instruction >> 5) & 0x1F;    // Registro base
    uint32_t imm9 = (instruction >> 12) & 0x1FF; // Immediate de 9 bits
    int32_t offset = imm9;  // Sign extend para obtener un valor de 32 bits

    // Verificar si el bit más significativo del offset está encendido
    if (offset & 0x100) {
        offset |= 0xFFFFFE00; // Extender el signo
    }

    // Calcular la dirección de memoria de destino
    uint32_t address = CURRENT_STATE.REGS[Rn] + offset;

    // Verificar si la dirección de memoria está dentro del rango permitido
    if (address < MEMORY_START_ADDRESS || address >= MEMORY_SIZE + MEMORY_START_ADDRESS) {
        // La dirección de memoria está fuera del rango permitido
        printf("Error: Dirección de memoria fuera de rango permitido.\n");
        return;
    }

    //recorto los primeros 16 bits que vamos a guardar
    uint32_t halfword_value = CURRENT_STATE.REGS[Rt] & 0xFFFF;

    // Escribir los primeros 16 bits de memoria en la dirección especificada
    mem_write_32(address, halfword_value);

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void LDUR(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rt = (instruction >> 0) & 0x1F;    // Registro destino
    uint32_t Rn = (instruction >> 5) & 0x1F;    // Registro base
    uint32_t imm9 = (instruction >> 12) & 0x1FF; // Immediate de 9 bits
    int32_t offset = imm9;  // No es necesario el sign extend

    // Calcular la dirección de memoria de origen
    uint32_t address = NEXT_STATE.REGS[Rn] + offset;

    // Verificar si la dirección de memoria está dentro del rango permitido
    if (address < MEMORY_START_ADDRESS || address >= MEMORY_SIZE + MEMORY_START_ADDRESS) {
        // La dirección de memoria está fuera del rango permitido
        printf("Error: Dirección de memoria fuera de rango permitido.\n");
        return;
    }

    // Leer 64 bits de memoria en la dirección especificada
    uint64_t value = mem_read_64(address);

    // Almacenar el valor obtenido en el registro destino
    NEXT_STATE.REGS[Rt] = value;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void LDURH(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rt = (instruction >> 0) & 0x1F;    // Registro destino
    uint32_t Rn = (instruction >> 5) & 0x1F;    // Registro base
    uint32_t imm9 = (instruction >> 12) & 0x1FF; // Immediate de 9 bits
    int32_t offset = imm9;  // No es necesario el sign extend

    // Calcular la dirección de memoria de origen
    uint64_t address = NEXT_STATE.REGS[Rn] + offset;

    // Verificar si la dirección de memoria está dentro del rango permitido
    if (address < MEMORY_START_ADDRESS || address >= MEMORY_SIZE + MEMORY_START_ADDRESS) {
        // La dirección de memoria está fuera del rango permitido
        printf("Error: Dirección de memoria fuera de rango permitido.\n");
        return;
    }

    // Leer 32 bits de memoria en la dirección especificada
    uint32_t value = mem_read_32(address);

    // Extraer los primeros 16 bits del valor obtenido
    uint32_t halfword_value = value & 0xFFFF;

    // Almacenar el valor obtenido en el registro destino
    NEXT_STATE.REGS[Rt] = halfword_value;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void LDURB(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rt = (instruction >> 0) & 0x1F;    // Registro destino
    uint32_t Rn = (instruction >> 5) & 0x1F;    // Registro base
    uint32_t imm9 = (instruction >> 12) & 0x1FF; // Immediate de 9 bits
    int32_t offset = imm9;  // No es necesario el sign extend

    // Calcular la dirección de memoria de origen
    uint64_t address = NEXT_STATE.REGS[Rn] + offset;

    // Verificar si la dirección de memoria está dentro del rango permitido
    if (address < MEMORY_START_ADDRESS || address >= MEMORY_SIZE + MEMORY_START_ADDRESS) {
        // La dirección de memoria está fuera del rango permitido
        printf("Error: Dirección de memoria fuera de rango permitido.\n");
        return;
    }

    // Leer 32 bits de memoria en la dirección especificada
    uint32_t value = mem_read_32(address);

    // Extraer los primeros 8 bits del valor obtenido
    uint32_t byte_value = value & 0xFF;

    // Almacenar el byte obtenido (zero-extendido) en el registro destino
    NEXT_STATE.REGS[Rt] = byte_value;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void MOVZ(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rd = (instruction >> 0) & 0x1F;  // Registro destino
    uint32_t imm = (instruction >> 5) & 0xFFFF; // Immediate de 16 bits
    //Asumimos que shift (hw=00)

    // Almacenar el inmediato en el registro destino
    NEXT_STATE.REGS[Rd] = imm;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void ADD_IMMEDIATE(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t imm = (instruction >> 10) & 0xFFF; // Valor Immediate de 12 bits
    uint32_t shift = (instruction >> 22) & 0x3; // Campo de desplazamiento
    uint32_t Rn = (instruction >> 5) & 0x1F; // Primer operando
    uint32_t Rd = (instruction & 0x1F); // Registro destino

    // Ajustar el valor Immediate según el campo de desplazamiento
    if (shift == 1) {
        imm <<= 12; // Desplazar 12 bits hacia la izquierda si se requiere
    }

    // Realizar la operación ADD y almacenar el resultado en el registro destino
    uint64_t result = NEXT_STATE.REGS[Rn] + imm;
    NEXT_STATE.REGS[Rd] = result;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}
void ADD_EXTENDED_REGISTER(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rm = (instruction >> 16) & 0x1F; // Segundo operando
    uint32_t Rn = (instruction >> 5) & 0x1F; // Primer operando
    uint32_t Rd = (instruction & 0x1F); // Registro destino
 
    // Realizar la operación ADD y almacenar el resultado en el registro destino
    uint64_t result = NEXT_STATE.REGS[Rn] + NEXT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void MUL(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rn = (instruction >> 5) & 0x1F; // Primer operando
    uint32_t Rm = (instruction >> 16) & 0x1F; // Segundo operando
    uint32_t Rd = (instruction >> 0) & 0x1F; // Registro destino
 
    // Realizar la operación MUL y almacenar el resultado en el registro destino
    uint64_t result = NEXT_STATE.REGS[Rn] * NEXT_STATE.REGS[Rm];
    NEXT_STATE.REGS[Rd] = result;

    // Actualizar program counter
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

void CBZ(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rt = instruction & 0x1F;
    uint64_t offset = ((instruction >> 5) & 0x7FFFF) << 2; // Campo de offset

    // Actualizar el program counter con la dirección de salto
    if (NEXT_STATE.REGS[Rt] == 0) {
        NEXT_STATE.PC = CURRENT_STATE.PC + offset;
    } else {
        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
    }
}

void CBNZ(uint32_t instruction) {
    // Extraer operandos y campos relevantes de la instrucción
    uint32_t Rt = instruction & 0x1F;
    uint64_t offset = ((instruction >> 5) & 0x7FFFF) << 2; // Campo de offset

    // Actualizar el program counter con la dirección de salto
    if (NEXT_STATE.REGS[Rt] != 0) {
        NEXT_STATE.PC = CURRENT_STATE.PC + offset;
    } else {
        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
    }
}

void process_instruction()
{
    /* execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory.
     * */

    // Fetch instruction
    uint32_t instruction = fetch(CURRENT_STATE.PC);
    
    // Decode instruction
    decode_instruction(instruction);

    //Execute instruction
    //execute_instruction(instruction);
}