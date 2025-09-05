implement a

domains
    parameters =
        parameters(
            string RuleSet,
            string CrewSolutionName [default("")],
            string CostFactorVersion [default("Master")],
            integer EvenAirborneHoursPenalty [default(0)],
            integer EvenCyclesPenalty [default(0)],
            boolean ConsiderHistoricHoursCycles [default(false)],
            integer BreakingOfrPenalty [default(500)],
            integer TurnTimeReduction [default(0)],
            boolean CheckBlockHours [default(true)],
            boolean CalculateBlockHours [default(false)],
            integer MaxDelay [default(0)],
            string DelayCode [default("")],
            integer DelayPenalty [default(3000)],
            integer DelayHourPenalty [default(3000)]).

end implement