# RNG-Testing
I've found it wacky when doing the RNG testing, with fewer documents, and less expertise.. thus, wanted to wrap something up for later uses.

# Intro
I've done for UK, Denmark, Sweden, Malta jurisdiction. Dieharder and NIST, Chi-squares tests are mostly wanted. I was struggling so badly when I first doing this, I'm a Security Engineer based so gotta do some math and try to understand the whole concept of the those tests, the p values and so on.

I urge you to read the documents carefully and precisely, otherwise, you might miss and do it wrong during the examination.

**Dieharder test suites**: http://webhome.phy.duke.edu/~rgb/General/dieharder.php

**NIST:** https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-22r1a.pdf

**Chi-square:** I'm using R-studio or matlab (https://statsandr.com/blog/chi-square-test-of-independence-in-r/) and (https://www.mathworks.com/help/stats/chi2gof.html)

# P-value Intepretation

The test statistic is used to calculate a P-value that summarizes the strength of the evidence against the null hypothesis. For these tests, each P-value is the probability that a perfect random number generator would have produced a sequence less random than the sequence that was tested, given the kind of non-randomness assessed by the test. If a P-value for a test is determined to be equal to 1, then the sequence appears to have perfect randomness. A P-value of zero indicates that the sequence appears to be completely non-random. A significance level (α) can be chosen for the tests. If P-value ≥ α, then the null hypothesis is accepted; i.e., the sequence appears to be random. If P-value < α, then the null hypothesis is rejected; i.e., the sequence appears to be non-random. 

We will use a 5% significance level (α = 0.05), indicating a 95% confidence level:

* α = 0.05: A P-value ≥ 0.05 suggests randomness with 95% confidence. A P-value < 0.05 indicates non-randomness with 95% confidence.

# The Tests

## 1. Dieharder Test suite

Please read this first and try to install the Diehard tool into your machine: http://webhome.phy.duke.edu/~rgb/General/dieharder.php


**File Input**
A formatted ASCII input file can accept either uints (integers in the range 0 to 2^31-1, one per line) or decimal uniform deviates with at least ten significant digits (that can be multiplied by UINT_MAX = 2^32 to produce a uint without dropping precision), also one per line. Floats with fewer digits will almost certainly fail bitlevel tests, although they may pass some of the tests that act on uniform deviates

**Ubuntu:**

`sudo apt-get update -y`

`sudo apt-get install -y Dieharder`

**Mac OSX:**
`brew install Dieharder` (isn't working anymore)

**Windows:**
[Download](https://webhome.phy.duke.edu/~rgb/General/dieharder.php) the latest version and install it

**Implementation**
1. Compiling the RNG code to get the output file which should be presented as an ASCII format in this case. If the output file saved in the binary format, we can just switch the option during the execution to binary “-g 201” or convert the output file to ASCII format by using the python script. I personally usually use binary format for the testing.

2. Run command 
`Dieharder -a -g 202 -f output.txt`

* -a runs all the tests with standard/default options to create a user-controllable report
* -f filename - generators 201 or 202 permit either raw binary or formatted  ASCII  numbers  to  be read in from a file for testing
* -g generator number - selects a specific generator for testing

3. As it has been mentioned earlier, the Dieharder suite is more than just the Diehard tests cleaned up and given a pretty GPL'd source face in native C. Tests from the Statistical Test Suite (STS) developed by the National Institute for Standards and Technology (NIST) are being incorporated, as are new tests developed by rgb. Therefore, we would just need to pay attention to the Diehard test results are shown in the figure below

Note: Increase the file size to check whether it passes all the tests.

## 2. NIST Test

Some notes when doing NIST:
- Should consist of a minimum of 1,000,000 bits to conduct all the test,
- Some tests, like the Overlapping Template Matching Test, need longer bitstreams (e.g., 10,000,000 bits). 
- More than 10 sequences should be defined, otherwise, value is undefined if less than 10 sequences are processed.
- The suite typically requires multiple bitstreams to perform statistical analysis effectively. A common recommendation is to use at least 55-100 bitstreams for reliable results.

**Bitstreams**
  
The term "bitstreams" in the context of the NIST test suite for RNGs often refers to the number of independent and identically distributed sequences that you want to test. The idea is to evaluate the randomness of not just a single sequence but multiple sequences to get a broader view of an RNG's performance. This is particularly useful when you want to understand the consistency of randomness across different sequences generated by the RNG.
Here's how you might approach this:
* Single Bitstream: If you only have one sequence of, say, 9,000,000 bits that you want to test, then you'd indicate that you have "1 bitstream".
* Multiple Bitstreams: If you have, for instance, three sequences each of 3,000,000 bits (totalling the 9,000,000 bits you mentioned earlier), then you would indicate that you have "3 bitstreams".
* Divide a Large Sequence: You could also take a large sequence and divide it into smaller chunks to create multiple bitstreams. For instance, if you have 9,000,000 bits, you could treat it as 9 separate bitstreams of 1,000,000 bits each or 18 bitstreams of 500,000 bits each, and so on.

**Example**


For a binary raw file input, the optimal configuration would balance the number of bitstreams and their length. Based on common practice and the requirements of the NIST tests, the following configuration is suggested:

	•	Length of Each Bitstream: 1,000,000 bits
	•	Number of Bitstreams: 100

This configuration provides a good balance, ensuring that each test has enough data to perform statistically significant analysis.

**Installation**
Ubuntu & MAC OSX:
[Download](https://csrc.nist.gov/projects/random-bit-generation/documentation-and-software) the software and compile with make

**Implementation**

For the examples in this document, α has been chosen to be 0.01. Note that, in many cases, the parameters in the examples do not conform to the recommended values; the examples are for illustrative purposes only.
1. Compiling the RNG code to get the output file which should be presented as an ASCII format in this case. If the output file saved in the binary format, we can just switch the option during the execution to binary or convert the output file to ASCII format by using the python script as described in Appendix A.

2. In order to invoke the NIST statistical test suite, type assess, followed by the desired bit stream length, n. For example, assess 100000. A series of menu prompts will be displayed in order to select the data to be analyzed and the statistical tests to be applied. The first screen appears as follows:

<img width="468" alt="image" src="https://github.com/enderphan94/RNG-Testing/assets/15033155/2cae31a4-bef5-4637-a539-ed8f830dc29f">

Select [0] to invoke the output file of the RNG which is located in data/data.pi
Once the user has prescribed a particular data set or PRNG, the statistical tests to be applied must be selected. The following screen is displayed:

<img width="447" alt="image" src="https://github.com/enderphan94/RNG-Testing/assets/15033155/034fa5d9-042b-4a82-82c4-a608c38a9b78">

In this case, 1 has been selected to indicate interest in applying all the subset of the available statistical tests.

<img width="447" alt="image" src="https://github.com/enderphan94/RNG-Testing/assets/15033155/87948788-2960-4204-bedd-1cb756240de8">

Ten sequences will be parsed using the data.pi file. Since a file was selected as the data specification mode, a subsequent query is made regarding the data representation. The user must specify whether the file consists of bits stored in ASCII format or hexadecimal strings stored in binary format.
Since the data consists of a long sequence of zeroes and ones, 0 was chosen. Given all necessary input parameters the test suite proceeds to analyze the sequences.

Once the testing process is complete, the empirical results can be found in the experiments/ subdirectory.

**ERRORS**

***igamc: UNDERFLOW***

The code is here: https://github.com/terrillmoore/NIST-Statistical-Test-Suite/blob/07cd57730402e9723b03f4775100773c36bfdc1b/sts/src/cephes.c#L32

The "igamc: UNDERFLOW" error occurs in the NIST Statistical Test Suite when the values being processed become too small for the statistical calculations used, especially in functions involving exponentials or logarithms, like the **complemented incomplete gamma function** igamc. Let’s analyze the primary causes based on the code and why increasing the max value resolves the issue.

**1\. Understanding the Complemented Incomplete Gamma Function (igamc)**

•The **complemented incomplete gamma function**, igamc(a, x), is sensitive to small values of x, especially when a is relatively larger.

•Inside the function, there is a term ax = a \* log(x) - x - lgam(a). If ax becomes very negative, exp(ax) (the exponential of ax) can underflow, leading to values smaller than floating-point precision can represent, resulting in a zero or underflow.

•The function has a threshold where, if ax < -MAXLOG (around -709), it returns 0.0 and logs "igamc: UNDERFLOW".

**2\. Why Small max Values Cause Underflow**

•When max is set to a small number (e.g., 100), the range of generated random numbers is constrained to small values, leading to bitstreams that have fewer high bits and more zeros.

•This bit-level distribution increases the likelihood of smaller values in the statistical tests, especially in bit patterns that may repeat or produce lower totals, causing x to be small in the igamc calculation.

•The statistical tests within NIST rely on having a sufficiently diverse distribution of bit values (both high and low). When constrained to smaller numbers, the lack of high bits skews the bit distribution, increasing the chances of underflow in functions expecting a uniform spread of values.

**3\. How Increasing max to Full 32-Bit Range Fixes the Issue**

•When max is set to the full 32-bit range (4294967295), the generated numbers fully utilize all bits in the 32-bit space, creating a highly uniform distribution of 0s and 1s across the bitstreams.

•This balanced bit distribution means x values in the statistical tests are spread out over a broader range, reducing the likelihood of x being too small.

•As a result, ax = a \* log(x) - x - lgam(a) is less likely to reach values that trigger underflow, allowing the igamc calculations to proceed without errors.

**Summary**

The "igamc: UNDERFLOW" error stems from:

1.**Small** max **Values**: Constraining max produces lower-bit values in the bitstream, creating a non-uniform distribution that skews statistical calculations.

2.**Sensitive Statistical Calculations**: Functions like igamc rely on a broad spread of values; small or zero-heavy distributions increase the risk of underflow.

If you must use min/max values, set them broadly enough to allow for a sufficiently diverse bit pattern distribution. Ideally, use ranges close to the maximum bit width, e.g., setting max to 2^32 - 1 in a 32-bit system.

## 3. Chi-square Test
1. Install R via brew

`$brew install R
`

2. [Install](https://www.rstudio.com/products/rstudio/download/) the latest version of RStudio

Syntax:

`chisq.test() is a function used to perform test.
`

Syntax of a chi-square test:

`chisq.test(data)`


Following is the description of the chi-square test parameters:
The input data is in the form of a table that contains the count value of the variables in the observation.
We use chisq.test function to perform the chi-square test of independence in the native stats package in R. For this test, the function requires the contingency table to be in the form of a matrix. Depending on the form of the data, to begin with, this can need an extra step, either combining vectors into a matrix or cross-tabulating the counts among factors in a data frame. 
We use read.table and as.matrix to read a table as a matrix. While using this, be careful of extra spaces at the end of lines. Also, for extraneous characters on the table, as these can cause errors.

**Evaluation**
The P-values is calculated and evaluated. We reject the null hypothesis if the p-value that comes out in the result is less than a predetermined significance level, which is 0.05 (the corresponding confidence level is 95%), then we reject the null hypothesis.
•	H0: The two variables are independent.
•	H1: The two variables relate to each other.

**Implementation**
1. Compiling the RNG code to produce an output file, which in this case should be in ASCII format (binary format is below) with each number separated by a new line to allow the program to read the data properly.

2. Import data and read data into a vector or list from the console or file.

`$ data <- scan("<directory>/<input-data-file>.txt")`

3. Test if the frequency of the drawn numbers is random. A barplot of the frequency of each number will help to get a better idea.
`$ barplot(table(data))`

<img width="452" alt="image" src="https://github.com/enderphan94/RNG-Testing/assets/15033155/3e1efe50-f021-4310-be5f-c7901de3cf70">
 
Figure 1 Frequency of each number

4. Run the Chi-Square test:

`$ chisq.test(table(data))`

Chi-squared test for given probabilities

`data:  table(data)`

`X-squared = 226.63, df = 255, p-value = 0.8991`

5. Evaluate the p-value

Th p-value is greater than 5% which means that we do not reject the null hypothesis which was that the distribution of the digit was independent.

**Dealing with binary file in R.**

if we’re dealing with raw unscaled random numbers direct from a generator, these are essentially raw entropy and aren't directly suited for a chi-squared test in the form they are in.

The chi-squared test is used to determine if there's a significant association between categorical variables by comparing observed counts to expected counts under the assumption of independence.

However, when working with raw random numbers (or random bits) from a generator, a chi-squared test can be applied in the context of a randomness test. This kind of test will let we determine if the output of our generator is behaving as expected (i.e., is it really random?).

To apply a chi-squared test in this context, we'll need to:

1. Load the .bin file.

`con <- readfile("boo.bin","rb")`

`data <- readBin(con, what = integer(), n = 1000000, endian = "little")`

`close(con)`

2. Bin the Numbers: Divide the range of possible values into a series of intervals or "bins". Each bin will then have an expected number of values if the generator is truly random. Ensure that the expected count in each bin is sufficiently large (e.g., >5) for the chi-squared test to be valid. For 9,000,000 draws, this would suggest around 3,000 bins (bin=9000000=3000)

`breaks <- seq(min(data), max(data), length.out = 3001)  # This creates 3000 bins`

3. Count Observed Values: For each bin, count the number of values from your generator that fall into that bin.

`observed_counts <- hist(data, breaks = breaks, plot = FALSE)$counts`

4. Calculate Expected Values: If the generator is truly random, each bin should have roughly the same number of values (or a number proportional to its range if the bins are of different sizes).

`expected_probs <- rep(1/length(breaks), length(breaks) - 1)`

`expected_probs <- expected_probs / sum(expected_probs)  # Ensure they sum up to 1`

5. Perform the Chi-Squared Test: Compare the observed counts to the expected counts using the chi-squared test.

`result <- chisq.test(observed_counts, p = expected_probs)`

`print(result)`

**Note:** We can read the first few numbers in R in various formats and see which one seems to produce reasonable values:

* Read as 32-bit integer (little-endian)

`int_values <- readBin("filename.bin", what = integer(), n = 25, endian = "little")`

`print(int_values)`

1. Read as 32-bit integer (big-endian)

`int_values_big <- readBin("filename.bin", what = integer(), n = 25, endian = "big")`

`print(int_values_big)`

* Read as 64-bit double (little-endian)

`double_values <- readBin("filename.bin", what = double(), n = 12, endian = "little")`

`print(double_values)`

* Read as 64-bit double (big-endian)

`double_values_big <- readBin("filename.bin", what = double(), n = 12, endian = "big")`

`print(double_values_big)`


# Swedish Testing Script

The following tests are implemented, the P-values are calculated and printed out:
- Chi-square

- Runs test

- Autocorrelation

`Rscript swedishTest.r <filename.bin> <numbers of raw>`

Expected results:

```
EnderM2:target enderphan$ Rscript swedishTest.R 600mdata.bin 600000000
Registered S3 method overwritten by 'quantmod':
  method            from
  as.zoo.data.frame zoo 
[1] "Ljung-Box Test Result:"

        Box-Ljung test

data:  ts_data
X-squared = 25.511, df = 20, p-value = 0.1826

[1] "Ljung-Box Test P-value: 0.182567008052608"
[1] "Chi-square Test Result:"

        Chi-squared test for given probabilities

data:  counts
X-squared = 2.1336e-07, df = 1, p-value = 0.9996

[1] "Chi-square Test P-value: 0.999631453146038"
[1] "Runs Test Result:"

        Runs Test

data:  as.factor(binary_sequence)
Standard Normal = 1.3778, p-value = 0.1683
alternative hypothesis: two.sided

[1] "Runs Test P-value: 0.168260062992897"
```

**Evaluation**

The output of the Chi-square test you’ve received indicates a very high p-value (0.9996). This suggests that there is no significant difference between the observed and expected frequencies of 0s and 1s, implying that the binary data could be considered random with respect to the expected distribution (equal numbers of 0s and 1s).

However, to determine the randomness of the data thoroughly, it’s best to look at the results of all three tests you’ve performed: the Ljung-Box test, the Chi-square test, and the Runs test. Each test assesses a different aspect of randomness:

1. Ljung-Box Test: This test checks for autocorrelation in the data. A high p-value (typically greater than 0.05) indicates that there is no significant autocorrelation, suggesting randomness.
2. Chi-square Test: This test checks if the observed frequencies of 0s and 1s match the expected frequencies (50% for each). A high p-value (typically greater than 0.05) indicates that the observed distribution does not significantly differ from the expected distribution, suggesting randomness.
3. Runs Test: This test checks for randomness in the sequence of data points by looking at the occurrence of runs (consecutive sequences of similar elements). A high p-value (typically greater than 0.05) indicates that the sequence of data points appears random.

**P-value Intepretation**

The test statistic is used to calculate a P-value that summarizes the strength of the evidence against the null hypothesis. For these tests, each P-value is the probability that a perfect random number generator would have produced a sequence less random than the sequence that was tested, given the kind of non-randomness assessed by the test. If a P-value for a test is determined to be equal to 1, then the sequence appears to have perfect randomness. A P-value of zero indicates that the sequence appears to be completely non-random. A significance level (α) can be chosen for the tests. If P-value ≥ α, then the null hypothesis is accepted; i.e., the sequence appears to be random. If P-value < α, then the null hypothesis is rejected; i.e., the sequence appears to be non-random. 

We will use a 5% significance level (α = 0.05), indicating a 95% confidence level:

* α = 0.05: A P-value ≥ 0.05 suggests randomness with 95% confidence. A P-value < 0.05 indicates non-randomness with 95% confidence.

