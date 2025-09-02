// BigNumber Demo by Yash Jain
// Copyright (c) 2025 Yash Jain. All rights reserved.
// This program implements large number arithmetic (BigNumber) and supports
// Addition, Subtraction, Multiplication, Division, Factorial, Fibonacci, and Catalan calculations.

#include <iostream>
#include <vector>
#include <string>
#include <stdexcept>
#include <algorithm>
#include <cctype>
using namespace std;

// Class representing arbitrarily large integers
class BigNumber {
    vector<int> digits;      // stores digits in reverse order (least significant digit first)
    bool isNegative = false; // sign flag

public:
    // Constructor from unsigned long long
    BigNumber(unsigned long long n = 0) {
        do { digits.push_back(n % 10); n /= 10; } while (n);
    }

    // Constructor from string
    BigNumber(const string &s) {
        digits.clear();
        isNegative = false;
        int start = 0;
        if (s.length() > 0 && s[0] == '-') { isNegative = true; start = 1; }
        for (int i = s.size() - 1; i >= start; i--) {
            if (!isdigit(s[i])) throw runtime_error("Invalid number");
            digits.push_back(s[i] - '0');
        }
        removeLeadingZeros();
    }

    // Copy constructor
    BigNumber(const BigNumber &b) { digits = b.digits; isNegative = b.isNegative; }

    // Remove unnecessary leading zeros
    void removeLeadingZeros() {
        while (digits.size() > 1 && digits.back() == 0) digits.pop_back();
        if (digits.size() == 1 && digits[0] == 0) isNegative = false;
    }

    // Check if number is zero
    bool isZero() const { return digits.size() == 1 && digits[0] == 0; }

    // Assignment operator
    BigNumber &operator=(const BigNumber &b) { digits = b.digits; isNegative = b.isNegative; return *this; }

    // Increment and decrement operators
    BigNumber &operator++() { *this = *this + BigNumber(1); return *this; }    // prefix
    BigNumber operator++(int) { BigNumber tmp(*this); ++(*this); return tmp; } // postfix
    BigNumber &operator--() { *this = *this - BigNumber(1); return *this; }    // prefix
    BigNumber operator--(int) { BigNumber tmp(*this); --(*this); return tmp; } // postfix

    // Equality operators
    bool operator==(const BigNumber &b) const { return digits == b.digits && isNegative == b.isNegative; }
    bool operator!=(const BigNumber &b) const { return !(*this == b); }

    // Compare absolute values
    bool absLess(const BigNumber &b) const {
        if (digits.size() != b.digits.size()) return digits.size() < b.digits.size();
        for (int i = digits.size() - 1; i >= 0; i--) if (digits[i] != b.digits[i]) return digits[i] < b.digits[i];
        return false;
    }

    // Relational operators
    bool operator<(const BigNumber &b) const {
        if (isNegative != b.isNegative) return isNegative;
        if (isNegative) return b.absLess(*this);
        return absLess(b);
    }
    bool operator>(const BigNumber &b) const { return b < *this; }
    bool operator<=(const BigNumber &b) const { return !(*this > b); }
    bool operator>=(const BigNumber &b) const { return !(*this < b); }

    // Unary minus operator
    BigNumber operator-() const { BigNumber res(*this); if (!res.isZero()) res.isNegative = !res.isNegative; return res; }

    // Addition operator
    BigNumber operator+(const BigNumber &b) const {
        if (isNegative != b.isNegative) return *this - (-b);
        BigNumber result; result.digits.clear(); result.isNegative = isNegative;
        int carry = 0, n = digits.size(), m = b.digits.size();
        for (int i = 0; i < max(n, m) || carry; i++) {
            int sum = carry + (i < n ? digits[i] : 0) + (i < m ? b.digits[i] : 0);
            result.digits.push_back(sum % 10);
            carry = sum / 10;
        }
        return result;
    }

    // Subtraction operator
    BigNumber operator-(const BigNumber &b) const {
        if (isNegative != b.isNegative) return *this + (-b);
        if (isNegative) return (-b) - (-(*this));
        if (absLess(b)) return -(b - *this);
        BigNumber result; result.digits.clear();
        int borrow = 0;
        for (int i = 0; i < digits.size(); i++) {
            int diff = digits[i] - borrow - (i < b.digits.size() ? b.digits[i] : 0);
            if (diff < 0) { diff += 10; borrow = 1; } else { borrow = 0; }
            result.digits.push_back(diff);
        }
        result.removeLeadingZeros(); return result;
    }

    // Multiplication operator
    BigNumber operator*(const BigNumber &b) const {
        BigNumber result; result.digits.assign(digits.size() + b.digits.size(), 0);
        result.isNegative = isNegative != b.isNegative;
        for (int i = 0; i < digits.size(); i++) {
            for (int j = 0; j < b.digits.size(); j++) {
                result.digits[i + j] += digits[i] * b.digits[j];
            }
        }
        int carry = 0;
        for (int i = 0; i < result.digits.size(); i++) {
            result.digits[i] += carry;
            carry = result.digits[i] / 10;
            result.digits[i] %= 10;
        }
        result.removeLeadingZeros(); return result;
    }

    // Division operator
    BigNumber operator/(const BigNumber &b) const {
        if (b.isZero()) throw runtime_error("Division by zero");
        BigNumber dividend = *this, divisor = b, quotient;
        bool resultSign = isNegative != b.isNegative;
        dividend.isNegative = divisor.isNegative = false;
        if (dividend.absLess(divisor)) return BigNumber(0);
        quotient.digits.resize(dividend.digits.size(), 0);
        BigNumber current;
        for (int i = dividend.digits.size() - 1; i >= 0; i--) {
            current.digits.insert(current.digits.begin(), dividend.digits[i]);
            current.removeLeadingZeros();
            int x = 0, l = 0, r = 9;
            while (l <= r) {
                int mid = (l + r) / 2;
                if (!(divisor * BigNumber(mid) > current)) { x = mid; l = mid + 1; }
                else r = mid - 1;
            }
            quotient.digits[i] = x;
            current = current - (divisor * BigNumber(x));
        }
        quotient.isNegative = resultSign;
        quotient.removeLeadingZeros();
        return quotient;
    }

    // Modulus operator
    BigNumber operator%(const BigNumber &b) const { return *this - (*this / b) * b; }

    // Output stream operator
    friend ostream &operator<<(ostream &out, const BigNumber &b) {
        if (b.isNegative && !b.isZero()) out << "-";
        for (int i = b.digits.size() - 1; i >= 0; i--) out << b.digits[i];
        return out;
    }

    // Input stream operator
    friend istream &operator>>(istream &in, BigNumber &b) { string s; in >> s; b = BigNumber(s); return in; }

    // Static caches for factorial, fibonacci, catalan
    static vector<BigNumber> factorialCache;
    static BigNumber factorial(int n) {
        if (factorialCache.empty()) factorialCache.push_back(BigNumber(1));
        for (int i = factorialCache.size(); i <= n; i++) factorialCache.push_back(factorialCache.back() * BigNumber(i));
        return factorialCache[n];
    }

    static vector<BigNumber> fibonacciCache;
    static BigNumber fibonacci(int n) {
        if (fibonacciCache.empty()) { fibonacciCache.push_back(BigNumber(0)); fibonacciCache.push_back(BigNumber(1)); }
        for (int i = fibonacciCache.size(); i <= n; i++) fibonacciCache.push_back(fibonacciCache[i - 1] + fibonacciCache[i - 2]);
        return fibonacciCache[n];
    }

    static vector<BigNumber> catalanCache;
    static BigNumber catalan(int n) {
        if (catalanCache.empty()) catalanCache.push_back(BigNumber(1));
        for (int i = catalanCache.size(); i <= n; i++)
            catalanCache.push_back(BigNumber::factorial(2 * i) / (BigNumber::factorial(i + 1) * BigNumber::factorial(i)));
        return catalanCache[n];
    }
};

// Initialize static caches
vector<BigNumber> BigNumber::factorialCache;
vector<BigNumber> BigNumber::fibonacciCache;
vector<BigNumber> BigNumber::catalanCache;

// Main program with menu
int main() {
    cout << "===== BigNumber Demo =====\n";
    int choice;
    while (true) {
        cout << "\nSelect an operation:\n";
        cout << "1. Addition\n2. Subtraction\n3. Multiplication\n4. Division\n5. Factorial\n6. Fibonacci\n7. Catalan\n8. Exit\nChoice: ";
        cin >> choice;
        if (choice == 8) break;

        if (choice >= 1 && choice <= 4) {
            BigNumber a, b;
            cout << "Enter first number: "; cin >> a;
            cout << "Enter second number: "; cin >> b;
            try {
                switch (choice) {
                    case 1: cout << "Result: " << a + b << endl; break;
                    case 2: cout << "Result: " << a - b << endl; break;
                    case 3: cout << "Result: " << a * b << endl; break;
                    case 4: cout << "Result: " << a / b << endl; break;
                }
            } catch (const exception &e) {
                cout << "Error: " << e.what() << endl;
            }
        } else if (choice >= 5 && choice <= 7) {
            int n; cout << "Enter n: "; cin >> n;
            if (n < 0) { cout << "Invalid input (n must be non-negative).\n"; continue; }
            switch (choice) {
                case 5: cout << "Factorial(" << n << ") = " << BigNumber::factorial(n) << endl; break;
                case 6: cout << "Fibonacci(" << n << ") = " << BigNumber::fibonacci(n) << endl; break;
                case 7: cout << "Catalan(" << n << ") = " << BigNumber::catalan(n) << endl; break;
            }
        } else {
            cout << "Invalid choice. Please try again.\n";
            break;
        }
    }
    return 0;
}
