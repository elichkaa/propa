package edu.kit.kastel.sdq.parallelsum;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class MaxOfMax {
    public int calculateMax(Collection<List<Integer>> blocksOfNumbers,
                            int numberThreads) throws ExecutionException, InterruptedException {

        if (blocksOfNumbers.size() == 0) {
            return Integer.MIN_VALUE;
        }

        List<Integer> results = new ArrayList<>();

        // Ergänzen Sie hier die Lösung für Aufgabe 5.1 des Übungsblatts

        return findMax(results);
    }

    private Integer findMax(Collection<Integer> numbers) {
        Integer maxValue = Integer.MIN_VALUE;
        for (Integer number : numbers) {
            if (number > maxValue) {
                maxValue = number;
            }
        }
        return maxValue;
    }

    private Integer findMaxStream(Collection<Integer> numbers) {
        // Ergänzen Sie hier die Lösung für Aufgabe 5.2 des Übungsblatts
    }
}
