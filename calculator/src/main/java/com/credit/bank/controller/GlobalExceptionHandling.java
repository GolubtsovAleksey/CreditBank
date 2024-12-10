package com.credit.bank.controller;

import com.credit.bank.dto.ErrorDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@Slf4j
@RestControllerAdvice
public class GlobalExceptionHandling {

    /**
     * Перехватывает все необработанные исключения и создает ответ с 500 http кодом.
     *
     * @param e необработанное исключение
     * @return ответ с 500 http кодом и кодом ошибки {@link HttpStatus#INTERNAL_SERVER_ERROR}
     */
    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorDto> serverError(final Exception e) {
        log.error("Internal server error:", e);

        final var responseDto = ErrorDto.builder()
                .message(e.getMessage())
                .code(HttpStatus.INTERNAL_SERVER_ERROR)
                .build();

        ResponseEntity<ErrorDto> errorDtoResponseEntity = ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)    // кидает 500 ошибку
                .body(responseDto);
        log.debug("Response to internal server error: {}", responseDto);
        return errorDtoResponseEntity;
    }
}

