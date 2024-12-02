package com.credit.bank.dto;

import java.math.BigDecimal;
import java.time.LocalDate;

import jakarta.validation.constraints.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ScoringDataDto {

    @NotNull
    @Min(value = 20000, message = "minimum credit 20000")
    private BigDecimal amount;

    @NotNull
    @Min(value = 6, message = "min loan term 6 month")
    private Integer term;

    @NotBlank
    @Size(min = 2, max = 30, message = "firstName should be between 2 and 30 characters")
    private String firstName;

    @NotBlank
    @Size(min = 2, max = 30, message = "lastName should be between 2 and 30 characters")
    private String lastName;

    @Size(min = 2, max = 30, message = "middleName should be between 2 and 30 characters")
    private String middleName;

    @NotNull
    private Gender gender;

    @NotNull
    @Past(message = "date of birth is not valid")
    private LocalDate birthdate;

    @NotNull
    @Size(min = 4, max = 4, message = "passport series consists of 4 digits")
    private String passportSeries;

    @NotNull
    @Size(min = 6, max = 6, message = "passport number consist of 6 digits")
    private String passportNumber;

    @NotNull
    @Past(message = "passport issue date is not valid")
    private LocalDate passportIssueDate;

    @NotBlank
    private String passportIssueBranch;

    @NotNull
    private MaritalStatus maritalStatus;

    @NotNull
    private Integer dependentAmount;

    @NotNull
    private EmploymentDto employment;

    @NotBlank
    private String accountNumber;

    @NotNull
    private Boolean isInsuranceEnabled;

    @NotNull
    private Boolean isSalaryClient;
}
