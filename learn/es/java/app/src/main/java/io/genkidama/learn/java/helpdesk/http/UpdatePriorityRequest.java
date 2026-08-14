package io.genkidama.learn.java.helpdesk.http;

import io.genkidama.learn.java.helpdesk.domain.TicketPriority;

/** Request payload for changing one ticket priority. @param priority required target priority */
public record UpdatePriorityRequest(TicketPriority priority) { }
