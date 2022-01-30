package se.alipsa.financials;

import org.junit.Test;
import org.renjin.script.RenjinScriptEngine;
import org.renjin.script.RenjinScriptEngineFactory;
import org.renjin.sexp.ListVector;
import org.renjin.sexp.SEXP;
import org.renjin.sexp.Vector;

import javax.script.ScriptException;

import static org.junit.Assert.assertEquals;

public class FinancialsTest {

   @Test
   public void testFinancials() throws ScriptException {
      RenjinScriptEngine engine = new RenjinScriptEngineFactory().getScriptEngine();
      engine.eval("library('se.alipsa:financials')");
      engine.put("loanAmount", 75000);
      engine.put("interestRate", 0.023);
      engine.put("tenureMonths", 24);
      SEXP sexp = (SEXP)engine.eval("pp <- paymentPlan(loanAmount, interestRate, tenureMonths)");
      ListVector df = (ListVector)sexp;
      Vector costOfCredit = (Vector)df.get("costOfCredit");
      double sumCostOfCredit = 0;
      for (int i = 0; i < costOfCredit.length(); i++) {
         sumCostOfCredit += costOfCredit.getElementAsDouble(i);
      }
      assertEquals("Java sum cost of credit", 76810.06, sumCostOfCredit, 0.01);
      double coc = ((SEXP)engine.eval("sum(pp$costOfCredit)")).asReal();
      assertEquals("R Sum cost of credit", coc, sumCostOfCredit, 0.01);
   }
}
