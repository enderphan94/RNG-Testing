# RNG-Testing
I've found it wacky when doing the RNG testing, with fewer documents, and less expertise.. thus, wanted to wrap something up for later uses.

# Intro
I've done for UK, Denmark, Sweden, Malta jurisdiction. Dieharder and NIST, Chi-squares tests are mostly wanted. I was struggling so badly when I first doing this, I'm a Security Engineer based so gotta do some math and try to understand the whole concept of the those tests, the p values and so on.

I urge you to read the documents carefully and precisely, otherwise, you might miss and do it wrong during the examination.

**Dieharder test suites**: http://webhome.phy.duke.edu/~rgb/General/dieharder.php

**NIST:** https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-22r1a.pdf

**Chi-square:** I'm using R-studio or matlab (https://statsandr.com/blog/chi-square-test-of-independence-in-r/) and (https://www.mathworks.com/help/stats/chi2gof.html)

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

## 2. NIST Test

Some notes when doing NIST:
- Should consist of a minimum of 1,000,000 bits to conduct all the test, 
- More than 10 sequences should be defined, otherwise, value is undefined if less than 10 sequences are processed. 
- Bitstreams
The term "bitstreams" in the context of the NIST test suite for RNGs often refers to the number of independent and identically distributed sequences that you want to test. The idea is to evaluate the randomness of not just a single sequence but multiple sequences to get a broader view of an RNG's performance. This is particularly useful when you want to understand the consistency of randomness across different sequences generated by the RNG.
Here's how you might approach this:
* 		Single Bitstream: If you only have one sequence of, say, 9,000,000 bits that you want to test, then you'd indicate that you have "1 bitstream".
* 		Multiple Bitstreams: If you have, for instance, three sequences each of 3,000,000 bits (totalling the 9,000,000 bits you mentioned earlier), then you would indicate that you have "3 bitstreams".
* 		Divide a Large Sequence: You could also take a large sequence and divide it into smaller chunks to create multiple bitstreams. For instance, if you have 9,000,000 bits, you could treat it as 9 separate bitstreams of 1,000,000 bits each or 18 bitstreams of 500,000 bits each, and so on.

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
