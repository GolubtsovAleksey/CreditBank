package com.credit.bank.service;

import com.credit.bank.dto.LoanOfferDto;
import com.credit.bank.dto.LoanStatementRequestDto;
import lombok.val;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.platform.commons.util.ReflectionUtils;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class OfferServiceTest {

    private ScoringService scoringService = new ScoringService();

    private OfferService offerService = new OfferService(scoringService);

    @BeforeEach
    void init() {
        ReflectionTestUtils.setField(scoringService, "baseCreditRate", 6);
    }

    @Test
    void generateOffer() {
        LoanStatementRequestDto requestDto = new LoanStatementRequestDto();
        requestDto.setBirthdate(LocalDate.now().minusYears(26));
        BigDecimal amount = new BigDecimal("20000");
        requestDto.setTerm(6);
        requestDto.setAmount(amount);

        List<LoanOfferDto> loanOfferDto = offerService.generateOffer(requestDto);

        assertEquals(4, loanOfferDto.size());
        assertEquals(amount, loanOfferDto.get(0).getRequestedAmount());                            // пров что amount находится в поле RequestedAmount возвр значLoanOfferDto из  List<LoanOfferDto> generateOffer

        BigDecimal totalAmountFalse = new BigDecimal("20000");                                    // mock метода из scoringService
        BigDecimal totalAmountTrue = new BigDecimal("20000");

        assertEquals(totalAmountFalse, loanOfferDto.get(0).getTotalAmount());                           // пров на то что totalAmount посчитался и пришёл в ответ
        assertEquals(totalAmountTrue, loanOfferDto.get(1).getTotalAmount());
        assertEquals(totalAmountFalse, loanOfferDto.get(2).getTotalAmount());
        assertEquals(totalAmountTrue, loanOfferDto.get(3).getTotalAmount());

        assertEquals(BigDecimal.valueOf(3), loanOfferDto.get(0).getRate());
        assertEquals(BigDecimal.valueOf(5), loanOfferDto.get(1).getRate());
        assertEquals(BigDecimal.valueOf(7), loanOfferDto.get(2).getRate());
        assertEquals(BigDecimal.valueOf(9), loanOfferDto.get(3).getRate());




    }
}