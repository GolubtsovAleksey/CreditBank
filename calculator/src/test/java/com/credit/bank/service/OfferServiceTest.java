package com.credit.bank.service;

import com.credit.bank.dto.LoanOfferDto;
import com.credit.bank.dto.LoanStatementRequestDto;
import lombok.val;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class OfferServiceTest {

    @Mock
    private ScoringService scoringService;

    @InjectMocks
    private OfferService offerService;


    @Test
    void generateOffer() {
        LoanStatementRequestDto requestDto = new LoanStatementRequestDto();
        BigDecimal amount = new BigDecimal("20000");
        requestDto.setTerm(6);
        requestDto.setAmount(amount);
        when(scoringService.calculateTotalAmount(any(), any())).thenReturn(new BigDecimal(20000));

        List<LoanOfferDto> loanOfferDto = offerService.generateOffer(requestDto);

        assertEquals(4, loanOfferDto.size());
        assertEquals(amount, loanOfferDto.get(0).getRequestedAmount());                            // пров что amount находится в поле RequestedAmount возвр значLoanOfferDto из  List<LoanOfferDto> generateOffer
        verify(scoringService, times(4)).calculateRate(any(), any());           // колич раз вызовов методов из scoringService
        verify(scoringService, times(4)).calculateTotalAmount(any(), any());

        BigDecimal totalAmountFalse = new BigDecimal("20000");                                    // mock метода из scoringService
        BigDecimal totalAmountTrue = new BigDecimal("20000");


        assertEquals(totalAmountFalse, loanOfferDto.get(0).getTotalAmount());                           // пров на то что totalAmount посчитался и пришёл в ответ
        assertEquals(totalAmountTrue, loanOfferDto.get(1).getTotalAmount());
        assertEquals(totalAmountFalse, loanOfferDto.get(2).getTotalAmount());
        assertEquals(totalAmountTrue, loanOfferDto.get(3).getTotalAmount());


    }
}