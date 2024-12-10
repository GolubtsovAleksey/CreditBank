package com.credit.bank.controller;

import com.credit.bank.dto.*;
import com.credit.bank.service.OfferService;
import com.credit.bank.service.ScoringService;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;


@Slf4j
@Validated
@RestController
@RequestMapping("/calculator")
@RequiredArgsConstructor
public class ControllerCalculator {

    @Autowired
    private final OfferService offerService;

    @Autowired
    private final ScoringService scoringService;

    @Operation(summary = "выполнение прескоринга данных")
    @PostMapping("/offers")
    public List<LoanOfferDto> calculatorOffers(@RequestBody @Valid LoanStatementRequestDto loanStatementRequestDto) {
        log.info("try to add loan offer {} ", loanStatementRequestDto.toString());
        return offerService.generateOffer(loanStatementRequestDto);
    }

    @Operation(summary = "выполнение скоринга данных")
    @PostMapping("/calc")
    public CreditDto calculatorCalc(@RequestBody @Valid ScoringDataDto scoringDataDto) {
        log.info("try to add credit {} ", scoringDataDto.toString());
        return scoringService.credit(scoringDataDto);
    }


}
