package com.credit.bank.dto;

import java.math.BigDecimal;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class EmploymentDto {

    @NotNull
    private EmploymentStatus employmentStatus;

    @NotBlank
    private String employerINN;

    @NotNull
    @Min(value = 1)
    private BigDecimal salary;

    @NotNull
    private Position position;

    @NotNull
    private Integer workExperienceTotal;

    @NotNull
    private Integer workExperienceCurrent;


}
