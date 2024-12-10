package com.credit.bank.service;

import com.credit.bank.dto.LoanOfferDto;
import com.credit.bank.dto.LoanStatementRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class OfferService {

    @Autowired
    private final ScoringService scoringService;

    public List<LoanOfferDto> generateOffer(LoanStatementRequestDto requestDto) {
        log.info("generate offer {} ", requestDto.toString());

        List<LoanOfferDto> createOfferList = new ArrayList<>();
        createOfferList.add(createOffer(true, true, requestDto));
        createOfferList.add(createOffer(true, false, requestDto));
        createOfferList.add(createOffer(false, true, requestDto));
        createOfferList.add(createOffer(false, false, requestDto));

//        createOfferList.sort(Comparator.comparing(LoanOfferDto::getIsInsuranceEnabled).thenComparing(LoanOfferDto::getIsSalaryClient));
        return createOfferList;
    }

    private LoanOfferDto createOffer(Boolean isInsuranceEnable,
                                     Boolean isSalaryClient,
                                     LoanStatementRequestDto requestDto) {

        log.info("create offer, isInsuranceEnable = {}, isSalaryClient = {} ", isInsuranceEnable, isSalaryClient);
        BigDecimal rate = scoringService.calculateRate(isInsuranceEnable, isSalaryClient);

        BigDecimal totalAmount = scoringService.calculateTotalAmount(
                requestDto.getBirthdate(),
                requestDto.getAmount());

        LoanOfferDto loanOfferDto = new LoanOfferDto();

        loanOfferDto.setStatementId(UUID.randomUUID());
        loanOfferDto.setRequestedAmount(requestDto.getAmount());
        loanOfferDto.setTotalAmount(totalAmount);
        loanOfferDto.setTerm(requestDto.getTerm());
        loanOfferDto.setMonthlyPayment(totalAmount.divide(BigDecimal.valueOf(requestDto.getTerm()), 2, RoundingMode.HALF_UP));
        loanOfferDto.setRate(rate);
        loanOfferDto.setIsInsuranceEnabled(isInsuranceEnable);
        loanOfferDto.setIsSalaryClient(isSalaryClient);

        log.info("created offer {} ", loanOfferDto);
        return loanOfferDto;
    }


}
