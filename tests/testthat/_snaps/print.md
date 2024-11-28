# print method produces output for single group

    Code
      print(mock_RtGam)
    Output
      ===============================
      Fitted RtGam model object (MockBackend)
      ===============================
      
      Model type: Adaptive (m = 2)
      Specified maximum smoothing basis dimension: 5 
      Family: poisson 
      Link function: log
      ===============================
      
      Observed data points: 10
      Distinct reference dates: 10
      Distinct groups: 1
      

# print method produces output for multiple groups

    Code
      print(mock_RtGam)
    Output
      ===============================
      Fitted RtGam model object (MockBackend)
      ===============================
      
      Model type: Adaptive (m = 2)
      Specified maximum smoothing basis dimension: 5 
      Family: poisson 
      Link function: log
      ===============================
      
      Observed data points: 10
      Distinct reference dates: 10
      Distinct groups: 2
      

# print method produces output for non-adaptive

    Code
      print(mock_RtGam)
    Output
      ===============================
      Fitted RtGam model object (MockBackend)
      ===============================
      
      Model type: Non-adaptive (m = 1)
      Specified maximum smoothing basis dimension: 5 
      Family: poisson 
      Link function: log
      ===============================
      
      Observed data points: 10
      Distinct reference dates: 10
      Distinct groups: 2
      

# Print method prints day of week effect

    Code
      print(mock_RtGam)
    Output
      ===============================
      Fitted RtGam model object (MockBackend)
      ===============================
      
      Model type: Non-adaptive (m = 1)
      Specified maximum smoothing basis dimension: 5 
      Family: poisson 
      Link function: log
      Using day-of-week effects
      ===============================
      
      Observed data points: 10
      Distinct reference dates: 10
      Distinct groups: 2
      Day-of-week levels: 10
      

