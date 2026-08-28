module enterprise_adapter_example
  implicit none
contains
  logical function run()
    integer :: legacy_code, legacy_cents, canonical_id
    real(8) :: amount
    legacy_code = 17; legacy_cents = 1250
    canonical_id = legacy_code
    amount = dble(legacy_cents) / 100.0d0
    run = canonical_id == 17 .and. abs(amount - 12.5d0) < 1.0d-9
  end function
end module
