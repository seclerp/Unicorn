# Unicorn

**Status**: in progress

High-level programming language with CIL backend. Work for bachelor degree

**Sample**:
```
// One line comments
/* Multi line comments */

// Modules
@ExplicitUsage // Attribute, ExplicitUsage prevents opening this module and forces yo use it name explicitly
module Sample {
    //Using namespaces
    open stdlib

    // Values
    let number: int = 0

    // Mutable values
    let mutable anotherNumber: int = 0
    anotherNumber = 5

    // Functions
    let sum (a: int) (b: int) : int {
        a + b
    }

    // Types
    // Record aka data-classes
    data Transaction {
        id: uuid
        amount: double
        userId: int
    }

    let transaction = Transaction { id = Uuid.new(); amount: 22.6; userId = 5 }

    // Algebraic data types
    // Unions aka sums
    union TransactionProvider {
        Facebook
        Google
        Ios
        Adyen
    }

    let txProvider = TransactionProvider.Facebook

    // Unions with payload, aka discriminated unions
    union Shape {
        Circle: int           // radius
        Rectangle: int * int  // a nd b sides
    }

    let shape = Shape.Circle 15

    // Single case union
    union UserId: string // equals to `union UserId { UserId : string }`

    // Aliases
    alias FirstName = string

    // Option types
    let validationInfo = Option.Some "This is example string"
    let noValue = Option.None

    // Pattern matching
    let calculateArea (shape: Shape) : double {
        match shape {
            (Shape.Circle radius) {
                radius ** 2 * math.Pi
            }
            (Shape.Rectangle (a, b)) {
                a * b
            }
        }
    }

    // Partial application
    let sum5 = sum 5
    let sum = sum5 6

    // Compose
    let sum1 = sum 1
    let sum2 = sum 2
    let sum12 = sum1 >> sum12
    let finalSum = sum12 5 // 8

    // Piping
    let result = 7 |> sum 3 |> sum 10 // 20
}
```