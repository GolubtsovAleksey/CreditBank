package com.credit.bank.dto;

import lombok.Builder;
import org.springframework.http.HttpStatus;

@Builder
public class ErrorDto {

    private String message;
    private HttpStatus code;


}
