import java.io.BufferedWriter;
import java.io.FileWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.io.IOException;
public class Processor {
	/*
	 * Maximize 6*Processor - 4*Percent that the human player makes the shot
	 */
	
	
	/*
	 * Variable for how often it bounces out(given it hits somthing else) 
	 * 1/2, 1/3??
	 * 
	 * if Q prob of bouncing (1-(k/(9Q)))
	 */
	static double throwAccuracy = 0.9;
	static double robotAccuracy = 0.9;
	static int scored = 12;
	static int available = 12;
	static int attempts = 18;
	
	public Processor(double throw, double robot, double scored, double attempts){
	  throwAccuracy = throw;
	  robotAccuracy = robot;
	  this.scored = scored;
	  this.attempts = attempts;
	  this.available = scored;
	}
	
	public static double[] runNet(){
	  double[] procReturns = new double[attempts];
		double[] netReturns = new double[attempts];
		int scored1 = 0;
		for (int i = 1; i <= attempts; i++) {
			scored1 = i;
			procReturns[i-1] = compute(scored1, throwAccuracy);
		}
		netReturns = Main.avgNet(Main.run(robotAccuracy, attempts, scored));
		return netReturns;
	}
	
		public static double[] runProc(){
	  double[] procReturns = new double[attempts];
		double[] netReturns = new double[attempts];
		int scored1 = 0;
		for (int i = 1; i <= attempts; i++) {
			scored1 = i;
			procReturns[i-1] = compute(scored1, throwAccuracy);
		}
		netReturns = Main.avgNet(Main.run(robotAccuracy, attempts, scored));
    return procReturns;
	}
	
	public static void main(String[] args) {
		double[] procReturns = new double[attempts];
		double[] netReturns = new double[attempts];
		int scored1 = 0;
		for (int i = 1; i <= attempts; i++) {
			scored1 = i;
			procReturns[i-1] = compute(scored1, throwAccuracy);
		}
		
		netReturns = Main.avgNet(Main.run(robotAccuracy, attempts, scored));
		System.out.print("Processor Returns: \n");
		print1DDouble(procReturns);
		System.out.println("\n");
		System.out.print("Net Returns: \n");
		print1DDouble(netReturns);
		writeToCSV(netReturns, "/Users/mitchellhung/scouting2025/netReturns.csv");
		writeToCSV(procReturns, "/Users/mitchellhung/scouting2025/procReturns.csv");
		
		
		System.out.println("Attempts || Net || Processor");
		for (int i = 0; i < attempts; i++) {
			System.out.println((i+1)+": "+ ((int) (netReturns[i]*1000))/1000.0+"  "+((int) (procReturns[i]*1000))/1000.0);
		}
	}
	
	public static double compute(int processor, double accuracy) {
		double result = 0.0;
		double[][] accuracyTable = Main.run(accuracy, attempts, scored);
		double[] expectedPointOutput = new double[processor];
		
		result += processor*6.0;
		
		for (int row = 0; row < processor; row++) {
			double total = 0;
			for (int col = 0; col < available; col++) {
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
}
