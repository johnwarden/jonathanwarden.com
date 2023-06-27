---
title: "Representationless Types"
slug: "representationless-types"
date: "2023-06-05T00:00:00"
math: true
---

## TODO

"precisionless" numbers


## Summary

In my essay on [functional equality](/functional-equality), I discuss situations where there are multiple representations of the same underlying value.


I suggest that language and library designers might define **representationless types** for situations where there are multiple ways of representing the same underlying value. 

For example, a `UniversalTime` type without a timezone would be different from the `LocalTime` type with a timezone. 

Because they are different types, a `UniversalTime` value could not be compared to a `LocalTime` value, even if the timezone of the `LocalTime` value happened to be GMT. But `LocalTime` would have a `UniversalTime()` method. So to see if two different `LocalTime` values in two different timezones represented the same actual moment in time, just compare their `UniversalTime` values:

{{< highlight java "linenos=false" >}}
localtime1.UniversalTime() == localtime2.UniversalTime()
{{< /highlight >}}


To represent a `UniversalTime` as a string, you would need to first convert it to a localtime by specifying a timezone:

{{< highlight java "linenos=false" >}}
localtime = universaltime.LocalTime("America/Los_Angeles")
println(localtime.Format(ISO8601))
{{< /highlight >}}

Similarly I would propose, a unitless Distance type, a NFC Unicode string type, etc.

### How to Represent Representationless Types

Now for most languages, every type needs to have the equivalent of a `toString` method (for debugging, etc). For representationless types, the `toString` method would need to take an argument specifying how it was to be represented, or this could come from the locale (local time zone). Or there could be one canonical representation (UTC, NTF, etc.)

For representationless data structures the string representation would be a dump of the full structure.


### Blinded Pointers

**Blinded pointers** would not reveal their address. The string representation of a blinded pointer would instead show the content being pointed to. Two different blinded pointers would be equal if their content was equal.

Mutating the content of a blinded pointer would have to be disallowed: two different variables can't be functionally equal if changing one doesn't have the same effect on program behavior as changing the other.

For example, suppose the Go language were modified such that pointers were blinded:

{{< highlight go "linenos=false" >}}

	type message struct {
		subject string
		body string
	}

	a := &message{ subject: "foo", body: "bar" }

	fmt.Println(a) 
	// output: &{foo bar}

	// NOT ALLOWED. Cannot reveal the address value of a pointer.
	// fmt.Printf("%p\n", a) 

	// NOT ALLOWED. Cannot mutate the content of a blinded pointer.
	a.body = "baz"

	b := &message{ subject: "foo", body: "bar" }

	fmt.Println(a == b)
	// output: true
	// unlike Go currently 

{{< /highlight >}}

### Representationless Numbers

A **representationless** `Number` type would be tricky, but possible. In practice, the result of numerical operations needs to have a finite precision; otherwise numerical values would tend to grow in size indefinitely. In fact, a precise representation of numbers such as `1/3` as a float would require an infinite amount of memory (`0.3333333...` and so on forever). At some point you have to round.

So you would not be able to do a simple division on a representationless numeric value unless you explicitly specified the precision of the division function to use.

For example, suppose a language had a representationless `Number` type which acted as a kind of interface type that hides the specifics of its internal representation. A value of a specific numeric type could be assigned to a variable of `Number` type, and two different `Number` values could be compared for equality even if they used a different internal representations. But operations on Numeric values would need to specify a type/precision.

{{< highlight java "linenos=false" >}}
Number a = (int64) 2+2 // a is internally represented by an int64
Number b = (float64) 4.0 // b is internally represented by a float64

// true, because they represent the same point on the number line
a == b 

// Divide by three using three different precisions
Number result1 = 1 /(float64) 3
Number result2 = 1 /(float32) 3
Number result3 = 1 /(int64) 3

// The result is three different values
println(result1) // Prints 0.3333333333333333
println(result2) // Prints 0.33333334
println(result2) // Prints 0


{{< /highlight >}}


Alternatively, instead of specifying prevision for every mathematical operation, the language could allow precision to be set for a scope. For example in the following code, the function `betaMean` is defined in terms of the representationless `Number` type, and does not specify the precision of mathematical operations it performs. Instead, the caller specifies the precision.


{{< highlight java "linenos=false" >}}

// Return the mean of a beta distribution with the given α and β parameters
function betaMean(Number α, Number β) Number
	return α / (α + β)
}

function main() {
	Number α = 1
	Number β  = 2
	set NumericPrecision=float64 {
		println(mean(α,β)) // Prints 0.3333333333333333
	}
	set NumericPrecision=float32 {
		println(mean(α,β)) // Prints 0.33333334
	}

}

{{< /highlight >}}

One example of a kind of representationless Numeric type are Go numeric constants. Though they only exist at the compiler level -- once assigned to a variable they must be represented some finite-precision numeric type.


## summary


TODO: why is representationless type better
  like a canonical value, but instead of selecting one of many possible representations as the canonical representation (e.g. GMT time, 64 bit float, etc), it completely separates the "underlying value" from its many representations.

  