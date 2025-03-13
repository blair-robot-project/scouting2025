import java.io.BufferedWriter;
import java.io.FileWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.IOException;


  //Notes: This is an imported java script that will generate probabilities/optimize 
  //net vs processor scoring
  
public class Main {
	
	static double robotAccuracy = 0.9;
	static double bounceOut = 0.66; //bounces out n/1.0 of the time
	
	public static void main(String[] args) {
		printFormattedArray(run(robotAccuracy,18, 12));	
		double[][] result = run(robotAccuracy, 18, 12);
		double[] expectedOutput = avgNet(result);
		
		System.out.println(Arrays.toString(expectedOutput));
		//writeToCSV(result, "/Users/mitchellhung/scouting2025/probabilities.csv");
		writeToCSV(expectedOutput, "/Users/mitchellhung/scouting2025/netReturns.csv");
	}
	
	public static double[] avgNet(double[][] arr) {
		double[] expectedOutput = new double[arr.length];
		int index = 0;
		for (double[] row: arr) {
			
			for (int i = 0; i < row.length; i++) {
				expectedOutput[index]+=((row[i]/100.0)*(i+1)*4);
			}
			index++;
		}
		return expectedOutput;
	}
	
	
	
    public static double[][] run(double P, int attempts, int goals) {
    	int totalNumberOfTries = attempts;
    	int goal = goals;
    	double[][] tableOfProbabilities = new double[totalNumberOfTries][goal];
    	
    	for (int a = 1; a <= totalNumberOfTries; a++) {
    		for (int b = 1; b <= goal; b++) {
    			tableOfProbabilities[a-1][b-1] = (double) ((int) (run2(a, b, P)*100000)) / 1000;
    		}
    	}
    	//printFormattedArray(tableOfProbabilities);
    	return tableOfProbabilities;
    }
    
    public static void writeToCSV(double[][] data, String filename) {
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(filename))) {
            // Write header row
            for (int col = 1; col <= data[0].length; col++) {
                writer.write("Goal_" + col + (col == data[0].length ? "\n" : ","));
            }

            // Write data rows
            for (double[] row : data) {
                for (int col = 0; col < row.length; col++) {
                    writer.write(String.format("%.3f", row[col]) + (col == row.length - 1 ? "\n" : ","));
                }
            }
            System.out.println("CSV file written successfully: " + filename);
        } catch (IOException e) {
            System.err.println("Error writing CSV: " + e.getMessage());
        }
    }
    
    public static void writeToCSV(double[] data, String filename) {
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(filename))) {
            // Write header
            writer.write("Value\n");
            
            // Write data rows
            for (double value : data) {
                writer.write(String.format("%.3f\n", value));
            }
            
            System.out.println("CSV file written successfully: " + filename);
        } catch (IOException e) {
            System.err.println("Error writing CSV: " + e.getMessage());
        }
    }
    
    
    
    
    
    public static double run2(int numberOfTries, int goal, double P) {
    	return sum(createProbability(createWinLossPattern(numberOfTries, goal), P));
    }

    
    public static double sum(ArrayList<Double> probabilities) {
    	double sum = 0.0;
    	for (Double item: probabilities) {
    		sum = sum + (double) item;
    	}
    	
    	return sum;
    }
    
    public static ArrayList<Double> createProbability(String[] allPatterns, double P) {
    	ArrayList<Double >result = new ArrayList<Double>(allPatterns.length);
    	for (String pattern: allPatterns) {
    		String[] letters = pattern.split("");
    		int numWins = 0;
    		double percentage = 1.0;
    		for (int i = 0; i < letters.length; i++) {
    			if (letters[i].equals("W")) {
    				percentage = percentage * (P * (1-numWins/9.0)+P*(numWins/9.0*bounceOut));
    				numWins++;
    			} else if (letters[i].equals("L")) {
    				percentage = percentage * (1-(P*(1-numWins/9.0)+P*(numWins/9.0*bounceOut)));
    			} else {
    				System.out.println("yes");
    			}
    		}  		
    		result.add(percentage);
    	}
    	return result;
    }
    
    public static String[] createWinLossPattern(int numberOfTries, int goal) {
        int length = binomial(numberOfTries, goal).intValue();
        String[] result = new String[length];
        
        // Temporary list to store results
        ArrayList<String> patterns = new ArrayList<>();
        generatePatterns("", numberOfTries, goal, patterns);

        // Convert list to array
        result = patterns.toArray(new String[0]);

        return result;
    }

    private static void generatePatterns(String currentPattern, int remainingTries, int remainingWins, ArrayList<String> patterns) {
        // Base case: No more tries left
        if (remainingTries == 0) {
            // Add pattern if we've hit the goal number of wins
            if (remainingWins == 0) {
                patterns.add(currentPattern);
            }
            return;
        }

        // Recursive case: Add "W" if we still need wins
        if (remainingWins > 0) {
            generatePatterns(currentPattern + "W", remainingTries - 1, remainingWins - 1, patterns);
        }

        // Add "L" for losses
        generatePatterns(currentPattern + "L", remainingTries - 1, remainingWins, patterns);
    }

    public static BigInteger binomial(int N, int K) {
        BigInteger ret = BigInteger.ONE;
        for (int k = 0; k < K; k++) {
            ret = ret.multiply(BigInteger.valueOf(N - k))
                     .divide(BigInteger.valueOf(k + 1));
        }
        return ret;
    }
    
    public static void printFormattedArray(double[][] array) {
        // Determine the column width based on the format
        int columnWidth = 10; // Adjust width to fit the largest numbers

        // Iterate through each row
        for (double[] row : array) {
            // Iterate through each column
            for (double value : row) {
                // Format each number to 3 decimal places and fixed width
                System.out.printf("%" + columnWidth + ".3f", value);
            }
            System.out.println(); // Move to the next line after each row
        }
    }

}