package ADT;

import ADT.IStack;

import java.util.ArrayList;
import java.util.List;

class Stack<T> implements IStack<T> {

    private List<T> stack;
    public Stack() {
        stack = new ArrayList<T>();
    }
    public void push(T item) {
        stack.add(item);
    }

    public T pop() {
        return stack.remove(stack.size()-1);
    }

    public T peek() {
        return stack.get(stack.size()-1);
    }

}