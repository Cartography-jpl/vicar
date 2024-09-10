!	Change log:
!
!	01-jan-90	Create wpt level 2 help file...dab
!
!
.KEY AMBIGVAL
Explanation:
                An "ambiguous" value was assigned to a variable.
                An ambiguous value is a value that is the left
                substring of more than one of the valid values
                for the variable.

User action:
                Determine the valid values for the variable (by
                requesting help on a parameter in tutor or by
                finding the declaration statement for the variable
                and consulting its "VALID" list), and reenter 
		the value.

.KEY BADIVAL 
Explanation:
		An invalid integer value has been specified.


User action:
		Determine the valid values for the variable (by
		requesting help on a parameter in tutor or by
		finding the declaration statement for the variable
		and consulting its "VALID" list), and reenter
		the value.

.KEY BADPNAME
Explanation:
		The parameter name that has been specified is
		not displayed in the given panel.

User action:
		Check your target for the given panel and reenter
		the value.

.KEY BADPVAL
Explanation:

		The given values are invalid and the parameter
		could not be updated.

User action:
		Determine the valid values for the variable (by
		requesting help on a parameter in tutor or by
		finding the declaration statement for the variable
		and consulting its "VALID" list), and reenter
		the value.

.KEY BADPVIEW
Explanation:
		The view of the parameter cannot be updated.

User action:
		Check your view information and then reenter the value.

.KEY BADRVAL
Explanation:

		An invalid real value has been specified.

User action:
		Determine the valid values for the variable (by
		requesting help on a parameter in tutor or by
		finding the declaration statement for the variable
		and consulting its "VALID" list), and reenter 
		the value.

.KEY BADVIEW
Explanation:
		A bad attribute was specifed so the item could
		not be created.


User action:
		Check you view information and then reenter the value.

.KEY INVALIDV
Explanation:
		An invalid value has been specified.

User action:
		Determine the valid values for the variable (by
		requesting help on a parameter in tutor or by
		finding the declaration statement for the variable
		and consulting its "VALID" list), and reenter 
		the value.


.KEY ITEMFAIL
Explanation:
		The item could not be created.

User action:
		Check all item target and view information.


.KEY OVRMAXC
Explanation:

                The number of values specified for a parameter
                is more than the maximum defined for the parameter.
		The excess will be ignored.

User action:

                If the variable is a parameter defined via the
                PARM statement, use HELP-PARM to determine the
                maximum number of values and re-issue the command
		accordingly.

                If the variable was defined in a proc via the
                LOCAL statement, inspect the PDF file and
                determine the allowable number of values by
                checking the COUNT field.

