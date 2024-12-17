package com.credit.bank.exception;

public class CreditUnavailableException extends RuntimeException {
    public CreditUnavailableException(String message) {
        super(message);
    }
}
