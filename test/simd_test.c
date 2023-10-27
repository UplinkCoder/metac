#include <stdint.h>
#include <assert.h>  // Include the assert header
/*
// Define a type for your SIMD vector
typedef struct {
    int16_t data[8];
} int16x8_t;

// Set all elements of a vector to a 16-bit value
int16x8_t Set1_16(int16_t value) {
    int16x8_t result;
    for (int i = 0; i < 8; i++) {
        result.data[i] = value;
    }
    return result;
}

// Perform element-wise bitwise AND operation between two vectors
int16x8_t And16(int16x8_t a, int16x8_t b) {
    int16x8_t result;
    for (int i = 0; i < 8; i++) {
        result.data[i] = a.data[i] & b.data[i];
    }
    return result;
}

// Compare equality of corresponding elements in two vectors and return a mask
int16x8_t Eq16(int16x8_t a, int16x8_t b) {
    int16x8_t result;
    for (int i = 0; i < 8; i++) {
        result.data[i] = (a.data[i] == b.data[i]) ? 0xFFFF : 0;
    }
    return result;
}

// Extract the most significant bit of each element in a vector and concatenate them into a 8-bit integer
uint32_t MoveMask16(int16x8_t a) {
    uint32_t result = 0;
    for (int i = 0; i < 8; i++) {
        if (a.data[i] & 0x8000) {
            result |= (1 << i);
        }
    }
    return result;
}

// Load 8 consecutive 16-bit values from memory address
int16x8_t Load16(const int16_t *ptr) {
    int16x8_t result;
    for (int i = 0; i < 8; i++) {
        result.data[i] = ptr[i];
    }
    return result;
}

// Store 8 consecutive 16-bit values to memory address
void Store16(int16_t *ptr, int16x8_t value) {
    for (int i = 0; i < 8; i++) {
        ptr[i] = value.data[i];
    }
}
*/
#include "../os/metac_simd.h"
int main() {
    {
        // Test Set1_16
        int16x8_t result = Set1_16(0x1234);  // Set all elements to 0x1234
        for (int i = 0; i < 8; i++) {
            assert(result.E[i] == 0x1234);
        }
    }
    {
        // Example data for testing
        int16x8_t result;
        int16x8_t vectorA, vectorB;
        for (int i = 0; i < 8; i++) {
            vectorA.E[i] = 0xAAAA;  // 1010101010101010 in binary
            vectorB.E[i] = 0x5555;  // 0101010101010101 in binary
        }

        // Test And16
        result = And16(vectorA, vectorB);
        for (int i = 0; i < 8; i++) {
            assert(result.E[i] == (0xAAAA & 0x5555));  // Result should be 0x0000
        }
    }

    // Test Eq16
    {
        int16x8_t vectorA = {1, 2, 3, 4, 5, 6, 7, 8};
        int16x8_t vectorB = {1, 0, 3, 4, 0, 6, 0, 8};
        int16x8_t expected = {0xFFFF, 0, 0xFFFF, 0xFFFF, 0, 0xFFFF, 0, 0xFFFF};

        int16x8_t result = Eq16(vectorA, vectorB);

        for (int i = 0; i < 8; i++) {
            assert(result.E[i] == expected.E[i]);  // Every even element should be all 1s
        }
    }
    {
        // Test MoveMask16
        int16x8_t result = Set1_16(0xFFFF);  // Create a vector with all bits set
        uint32_t mask = MoveMask16(result);
        assert(mask == 0xFF);  // All 8 first bits should be set
    }
    // Use the result and mask as needed

    // Example of loading and storing
    int16x8_t loadedData;
    for (int i = 0; i < 8; i++) {
        loadedData.E[i] = 0;
    }
    int16x8_t loadedVector = Load16(&loadedData);

    // Make assertions for the loaded data
    for (int i = 0; i < 8; i++) {
        assert(loadedVector.E[i] == loadedData.E[i]);
    }

    int16x8_t storeData;
    for (int i = 0; i < 8; i++) {
        storeData.E[i] = 0xBBBB;
    }
    Store16(&loadedData, storeData);

    // Make assertions for the stored data
    for (int i = 0; i < 8; i++) {
        assert(loadedData.E[i] == storeData.E[i]);
    }

    return 0;
}

