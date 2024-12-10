package com.credit.bank.service;

import com.credit.bank.dto.CreditDto;
import com.credit.bank.dto.EmploymentDto;
import com.credit.bank.dto.PaymentScheduleElementDto;
import com.credit.bank.dto.ScoringDataDto;
import com.credit.bank.exception.CreditUnavailableException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import static com.credit.bank.dto.EmploymentStatus.BUSINESS;
import static com.credit.bank.dto.EmploymentStatus.UNEMPLOYED;
import static com.credit.bank.dto.MaritalStatus.MARRIED;
import static com.credit.bank.dto.Position.TOP_MANAGER;

@Slf4j
@Service
public class ScoringService {

    @Value("${application.calculator.baseCreditRate}")
    private Integer baseCreditRate;

    private static final BigDecimal ONE_HUNDRED = new BigDecimal("100");

    public BigDecimal calculateRate(Boolean isInsuranceEnable, Boolean isSalaryClient) {
        if (Boolean.FALSE.equals(isInsuranceEnable) && Boolean.FALSE.equals(isSalaryClient)) {
            return BigDecimal.valueOf(((long) baseCreditRate + 3));
        } else if (Boolean.FALSE.equals(isInsuranceEnable) && Boolean.TRUE.equals(isSalaryClient)) {
            return BigDecimal.valueOf(((long)baseCreditRate + 1));
        } else if (Boolean.TRUE.equals(isInsuranceEnable) && Boolean.FALSE.equals(isSalaryClient)) {
            return BigDecimal.valueOf(((long)baseCreditRate - 1));
        } else {
            return BigDecimal.valueOf(((long)baseCreditRate - 3));
        }
    }

    public BigDecimal calculateTotalAmount(LocalDate birthdate, BigDecimal amount) {
        if (birthdate.isAfter(LocalDate.now().minusYears(25)))
            return percentOf(BigDecimal.valueOf(20), amount);
        else
            return amount;
    }

    public BigDecimal percentOf(BigDecimal percentage, BigDecimal total) {
        return percentage.multiply(total).divide(ONE_HUNDRED, 2, RoundingMode.HALF_UP);
    }

    public void scoringRequestValidation(ScoringDataDto scoringDataDto) {
        if (scoringDataDto.getBirthdate().isBefore(LocalDate.now().minusYears(65))) {
            throw new CreditUnavailableException("credit not available, age over 65");
        }
        if (scoringDataDto.getEmployment().getEmploymentStatus().equals(UNEMPLOYED)) {
            throw new CreditUnavailableException("credit not available, not employed");
        }
        if (scoringDataDto.getDependentAmount() >= 2) {
            throw new CreditUnavailableException("credit not available, invalid value - dependentAmount >= 2");
        }
        EmploymentDto employmentDto = scoringDataDto.getEmployment();

        if (employmentDto.getWorkExperienceTotal() < 1) {
            throw new CreditUnavailableException("credit not available, invalid value - workExperienceTotal < 1");
        }
        if (employmentDto.getWorkExperienceCurrent() < 0.5) {
            throw new CreditUnavailableException("credit not available, invalid value - orkExperienceCurrent < 0.5");
        }
    }

    public CreditDto credit(ScoringDataDto scoringDataDto) {
        scoringRequestValidation(scoringDataDto);

        EmploymentDto employmentDto = scoringDataDto.getEmployment();
        BigDecimal amount = calculateTotalAmount(scoringDataDto.getBirthdate(), scoringDataDto.getAmount());
        BigDecimal rate = calculateRate(scoringDataDto.getIsInsuranceEnabled(), scoringDataDto.getIsSalaryClient());

        if (scoringDataDto.getMaritalStatus().equals(MARRIED)) {
            rate = rate.subtract(BigDecimal.valueOf(0.3));
        } else {
            rate = rate.add(BigDecimal.valueOf(0.2));
        }
        if (scoringDataDto.getEmployment().getEmploymentStatus().equals(BUSINESS)) {
            rate = rate.subtract(BigDecimal.valueOf(1));
        }
        if (scoringDataDto.getDependentAmount().equals(1)) {
            amount = percentOf(BigDecimal.valueOf(10), amount);
        }
        if (employmentDto.getPosition().equals(TOP_MANAGER)) {
            rate = rate.subtract(BigDecimal.valueOf(0.1));
        }
        CreditDto creditDto = new CreditDto();
        creditDto.setAmount(amount);
        creditDto.setTerm(scoringDataDto.getTerm());
        creditDto.setRate(rate);
        creditDto.setIsInsuranceEnabled(scoringDataDto.getIsInsuranceEnabled());
        creditDto.setIsSalaryClient(scoringDataDto.getIsSalaryClient());
        creditDto.setPaymentSchedule(monthlyPaymentSchedule(rate, amount, scoringDataDto.getTerm()));

//        BigDecimal loanInterestCalculation = loanInterestCalculation(rate, amount);              // расчёт % по кредиту
        BigDecimal paymentPerMonth = paymentPerMonth(amount, scoringDataDto.getTerm(), rate);            // плата в месяц
        BigDecimal calculatePck = calculatePck(BigDecimal.valueOf(scoringDataDto.getTerm()), paymentPerMonth, amount);                          // расчёт пск

        if (employmentDto.getSalary().divide(BigDecimal.valueOf(2), 2, RoundingMode.HALF_UP).compareTo(paymentPerMonth) != 1) {
            throw new CreditUnavailableException("credit not available, salary does not meet the requirements");
        }
        creditDto.setMonthlyPayment(paymentPerMonth);
        creditDto.setPsk(calculatePck);

        log.info("create credit {} ", creditDto);
        return creditDto;
    }

    private List<PaymentScheduleElementDto> monthlyPaymentSchedule(BigDecimal paymentPerMonth, BigDecimal totalAmount, int term) {  // расчет графика платежей по кредиту
        List<PaymentScheduleElementDto> paymentScheduleElementDtoList = new ArrayList<>();
        for (int fullTerm = 0; fullTerm < term; fullTerm++) {
            PaymentScheduleElementDto scheduleElementDto = new PaymentScheduleElementDto();
            BigDecimal remainingDebt = totalAmount.subtract(paymentPerMonth);
            scheduleElementDto.setRemainingDebt(remainingDebt);
            paymentScheduleElementDtoList.add(scheduleElementDto);
        }
        return paymentScheduleElementDtoList;
    }

    private BigDecimal loanInterestCalculation(BigDecimal rate, BigDecimal amount) {   // сумма процента переплаты в месяц
        rate = rate.divide(BigDecimal.valueOf(100), 4, RoundingMode.HALF_UP);
        BigDecimal loanInterestCalc = amount.multiply(rate).divide(BigDecimal.valueOf(12), 2, RoundingMode.HALF_UP);
        return loanInterestCalc;
    }

    private BigDecimal paymentPerMonth(BigDecimal totalAmount, Integer term, BigDecimal rate) {   //Формула расчета аннуитетного платежа:
        BigDecimal monthlyRate = rate.divide(BigDecimal.valueOf(12), 2, RoundingMode.HALF_UP);
        BigDecimal addAction1 = monthlyRate.add(BigDecimal.valueOf(1));
        BigDecimal K = BigDecimal.valueOf(Math.pow(addAction1.doubleValue(), term));
        BigDecimal action3 = monthlyRate.multiply(K);
        BigDecimal action4 = K.subtract(BigDecimal.valueOf(1));
        BigDecimal KA = action3.divide(action4, 2, RoundingMode.HALF_UP);
        BigDecimal action6 = totalAmount.multiply(KA);
        return action6;

    }

    private BigDecimal calculatePck(BigDecimal term,                           // вычисляет пск
                                    BigDecimal paymentPerMonth,
                                    BigDecimal amount) {
        BigDecimal action1 = term.divide(BigDecimal.valueOf(12), 2, RoundingMode.HALF_UP);
        BigDecimal action2 = paymentPerMonth.multiply(term).divide(amount, 2, RoundingMode.HALF_UP).subtract(BigDecimal.valueOf(1));
        BigDecimal action3 = action2.divide(action1, 2, RoundingMode.HALF_UP);
        BigDecimal action4 = action3.multiply(BigDecimal.valueOf(100));
        return action4;

    }
}
