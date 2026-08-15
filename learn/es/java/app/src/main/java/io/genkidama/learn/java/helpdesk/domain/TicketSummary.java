package io.genkidama.learn.java.helpdesk.domain;

/**
 * Immutable operational counts derived from the current ticket snapshot.
 * Values are computed on demand and do not mutate or persist ticket state.
 *
 * @param total total tickets
 * @param open tickets in OPEN
 * @param inProgress tickets in IN_PROGRESS
 * @param resolved tickets in RESOLVED
 * @param low LOW priority tickets
 * @param normal NORMAL priority tickets
 * @param high HIGH priority tickets
 */
public record TicketSummary(
        long total,
        long open,
        long inProgress,
        long resolved,
        long low,
        long normal,
        long high) { }
