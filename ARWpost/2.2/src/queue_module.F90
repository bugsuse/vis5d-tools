!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module: queue_module
!
! Description: This module implements a queue of user-defined data types and 
!   a set of routines related to the maintenance and manipulation of the queue.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE queue_module

   USE module_debug

   type q_data         ! The user-defined datatype to store in the queue
      integer                :: x, y
      character (len=128)    :: units, description, stagger
   end type q_data
 
   type q_item         ! Wrapper for item to be stored in the queue
      type (q_data)          :: data
      type (q_item), pointer :: next
   end type q_item
 
   type queue          ! The queue object, defined by a head and tail pointer
      type (q_item), pointer :: head, tail
      integer                :: length
   end type queue
 
   CONTAINS
 
  
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_init
   ! Purpose: To initialize a queue
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE q_init(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(inout) :: q
  
      NULLIFY(q%head)
      NULLIFY(q%tail)
      q%length = 0
 
   END SUBROUTINE q_init
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_insert
   ! Purpose: To insert an item in the tail of the queue
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE q_insert(q, qitem)
    
      implicit none
  
      ! Arguments
      type (queue), intent(inout) :: q
      type (q_data), intent(in)   :: qitem
  
      ! Local variables
      type (q_item), pointer      :: newitem
  
      allocate(newitem)
      newitem%data = qitem
      NULLIFY(newitem%next) 
      IF (.not.associated(q%tail)) THEN
         q%head=>newitem
         q%tail=>newitem
      ELSE
         q%tail%next=>newitem
         q%tail=>newitem
      END IF
  
      q%length = q%length + 1
 
   END SUBROUTINE q_insert
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_isdata
   ! Purpose: This function returns FALSE if the queue is empty and TRUE otherwise 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function q_isdata(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(in) :: q
  
      ! Local variables
      logical                  :: q_isdata
  
      q_isdata = .false.
    
      IF (associated(q%head) .and. (q%length >= 1)) THEN 
         q_isdata = .true.
      END IF
 
   END function q_isdata
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_peek
   ! Purpose: To return the item in the head of the queue, without
   !    actually removing the item 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function q_peek(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(in) :: q
  
      ! Local variables
      type (q_data)            :: q_peek
  
      IF (associated(q%head)) THEN
         q_peek = q%head%data 
      ELSE
         call mprintf(.true.,ERROR,'q_peek(): Trying to peek at an empty queue')
      END IF
 
   END function q_peek
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_length
   ! Purpose: To return the number of items currently in the queue
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   FUNCTION q_length(q)
   
      implicit none
  
      ! Arguments
      type (queue), intent(in) :: q
  
      ! Local variables
    !!type (q_item), pointer   :: cursor
      integer                  :: q_length      
  
      q_length = q%length
  
  ! USE THE FOLLOWING TO COUNT THE LENGTH BY ACTUALLY TRAVERSING THE LINKED LIST
  ! REPRESENTATION OF THE QUEUE
  !    IF (associated(q%head)) THEN
  !       q_length = q_length + 1
  !       cursor=>q%head
  !       DO WHILE(associated(cursor%next))
  !         cursor=>cursor%next
  !         q_length = q_length + 1
  !       END DO
  !    END IF
 
   END FUNCTION q_length
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_remove
   ! Purpose: To return the item stored at the head of the queue
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function q_remove(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(inout) :: q
  
      ! Local variables
      type (q_data)               :: q_remove
      type (q_item), pointer      :: cursor
       
      IF (associated(q%head)) THEN
         IF (associated(q%head%next)) THEN
            cursor=>q%head%next
            q_remove = q%head%data
            deallocate(q%head)
            q%head=>cursor
         ELSE
            q_remove = q%head%data
            deallocate(q%head)
            NULLIFY(q%head)
            NULLIFY(q%tail)
         END IF 
         q%length = q%length - 1
      ELSE
         CALL mprintf(.true.,ERROR,'q_remove(): Trying to remove from an empty queue')
      END IF
 
   END function q_remove
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: q_destroy
   ! Purpose: To free all memory allocated by the queue, thus destroying any 
   !    items that have not been removed
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE q_destroy(q)
 
      implicit none
  
      ! Arguments
      type (queue), intent(inout) :: q
  
      ! Local variables
      type (q_item), pointer      :: cursor
  
      q%length = 0
  
      IF (associated(q%head)) THEN
         DO WHILE(associated(q%head%next))
            cursor=>q%head
            q%head=>q%head%next
            deallocate(cursor)
         END DO
         deallocate(q%head)
      END IF
 
   END SUBROUTINE q_destroy

END MODULE queue_module
