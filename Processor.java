import java.io.BufferedWriter;
import java.io.FileWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.io.IOException;
public class Processor {
	/*
	 * Maximize 6*Processor - 4*Percent that the human player makes the shot
	 */
	
	public static void main(String[] args) {
		double throwAccuracy = 0.75;
		int scored = 9;
		double[] returns = new double[13];
		
		for (int i = 1; i <= 13; i++) {
			scored = i;
			returns[i-1] = compute(scored, throwAccuracy);
		}
		
		print1DDouble(returns);
		writeToCSV(returns, "/Users/mitchellhung/scouting2025/probability2.csv");
	}
	
	public static double compute(int processor, double accuracy) {
		double result = 0.0;
		double[][] accuracyTable = Main.run();
		double[] expectedPointOutput = new double[processor];
		
		result += processor*6.0;
		
		for (int row = 0; row < processor; row++) {
			double total = 0;
			for (int col = 0; col < 9; col++) {
				total += accuracyTable[row][col]/100.0*(4*(col+1));
			}
			expectedPointOutput[row] = total;
			//System.out.println(total);
		}
		
		result -= expectedPointOutput[processor-1];
		
		return result;
	}
	
	
	public static void print1DDouble(double[] returns) {
		System.out.print(returns[0]+", "+returns[1]+", "+returns[2]+", \n"+returns[3]+", "+returns[4]+", "+
				returns[5]+", \n"+returns[6]+", "+returns[7]+", "+returns[8]+", \n"+returns[9]+", "+
				returns[10]+", "+returns[11]+", \n"+returns[12]);
	}
	
	public static void writeToCSV(double[] data, String filename) {
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(filename))) {
            // Write header row
            for (int col = 1; col <= data.length; col++) {
                writer.write("Goal_" + col + (col == data.length ? "\n" : ","));
            }

            // Write data row
            for (int item = 0; item < data.length; item++) {
                writer.write(String.format("%.3f", data[item]) + (item == data.length - 1 ? "\n" : ","));
            }
            
            System.out.println("CSV file written successfully: " + filename);
        } catch (IOException e) {
            System.err.println("Error writing CSV: " + e.getMessage());
        }
    }
	
}
